module Data.Parsnip (
    Parser (..),
    ParseSuccess (..),
    ParseFailure (..),
    ParseError (..),
    some,
    none,
    satisfy,
    single,
    sequence,
    from,
    rethrow,
    alternatives,
    optional,
    times,
    many,
    fallback,
    delimited,
) where

import Prelude hiding (sequence)

import Control.Applicative (Alternative (empty, (<|>)))

import Data.Foldable (asum)

-- | @`ParseSuccess` i o@ represents a successful parse with output of type @o@ and remaining input of type @i@.
data ParseSuccess i o = Success
    { -- | The parsed output.
      output :: o,
      -- | The remaining input to parse.
      remaining :: i,
      -- | The amount of input that was consumed.
      offset :: Int
    }
    deriving (Show)

success o i n = Right (Success o i n)

-- | @`ParseFailure` i e@ represents a failed parse with a list of errors of type @e@ and bad input of type @i@.
data ParseFailure i e = Failure
    { -- | A list of errors containing information about why the parse failed.
      errors :: [e],
      -- | The input that caused the parsing to fail.
      input :: i,
      -- | The position in the input where the failure occurred.
      position :: Int
    }
    deriving (Show)

failure e i n = Left (Failure e i n)

-- | @`Parser` i e o@ is a parser that accepts input of type @i@ and produces either errors of type @e@ or output of type @o@.
newtype Parser i e o = Parser
    { -- | `parse` takes an input of type @i@ and either fails (returning a `ParseFailure` @i@ @e@) or succeeds (returning a `ParseSuccess` @i@ @o@).
      parse :: i -> Either (ParseFailure i e) (ParseSuccess i o)
    }

instance Functor (Parser i e) where
    fmap f (Parser p) = Parser (p' . p)
        where
            p' (Left l) = Left l
            p' (Right r) = success (f (output r)) (remaining r) (offset r)

instance Monad (Parser i e) where
    return = pure

    Parser p >>= f = Parser p'
        where
            p' i = do
                x <- p i
                case parse (f (output x)) (remaining x) of
                    Left l -> failure (errors l) (input l) (offset x + position l)
                    Right r -> success (output r) (remaining r) (offset x + offset r)

instance Applicative (Parser i e) where
    pure o = Parser (\i -> success o i 0)

    Parser pl <*> Parser pr = Parser p'
        where
            p' i = do
                xl <- pl i
                xr <- pr (remaining xl)
                return (Success (output xl (output xr)) (remaining xr) (offset xl + offset xr))

instance Alternative (Parser i e) where
    empty = Parser (\i -> failure [] i 0)

    Parser pl <|> Parser pr = Parser p'
        where
            p' i = case pl i of
                Right r -> Right r
                Left l -> case pr i of
                    Right r' -> Right r'
                    Left l' -> failure (errors l ++ errors l') i 0

-- | A parse error type that covers most basic error cases.
data ParseError i
    = -- | Expected some input, but got nothing.
      Expected
    | -- | Expected no input, but got @i@.
      Unexpected i
    | -- | Expected some input satisfying a predicate, but got nothing or input did not satisfy the predicate.
      Unsatisfied (Maybe i)
    | -- | Expected some input equal to @i@, but got nothing or input was not equal.
      Unequal i (Maybe i)
    deriving (Show)

-- | @`some`@ succeeds on the next input from a list, failing with a `ParseError` if the list was empty.
some :: Parser [i] (ParseError i) i
some = Parser p
    where
        p [] = failure [Expected] [] 0
        p (hd : tl) = success hd tl 1

-- | @`none`@ succeeds on an empty list, failing with a `ParseError` otherwise.
none :: Parser [i] (ParseError i) ()
none = Parser p
    where
        p [] = success () [] 0
        p (hd : tl) = failure [Unexpected hd] (hd : tl) 0

-- | @`satisfy` f@ succeeds on the next input (from a list) satisfying the predicate @f@, failing with a `ParseError` if @f@ returned `False`.
satisfy :: (i -> Bool) -> Parser [i] (ParseError i) i
satisfy f = Parser p
    where
        p [] = failure [Unsatisfied Nothing] [] 0
        p (hd : tl)
            | f hd = success hd tl 1
            | otherwise = failure [Unsatisfied (Just hd)] (hd : tl) 0

-- | @`single` x@ succeeds on the next input (from a list) if it equals @x@, failing with a `ParseError` otherwise.
single :: Eq i => i -> Parser [i] (ParseError i) i
single x = rethrow (\(Unsatisfied y) -> Unequal x y) (satisfy (== x))

-- | @`sequence` xs@ succeeds on the next multiple elements from a list if they equal the elements of @xs@ in the same order.
sequence :: Eq i => [i] -> Parser [i] (ParseError i) [i]
sequence xs = f xs []
    where
        f [] o = return (reverse o)
        f (hd : tl) o = do
            x <- single hd
            f tl (hd : o)

-- | @`from` xs@ maps each element @x@ in @xs@ to the parser @single x@ and tries each one until the first successful parse.
from :: Eq i => [i] -> Parser [i] (ParseError i) i
from = alternatives . map single

-- | @`rethrow` f p@ maps the errors in the parser @p@ to another error type using @f@.
rethrow :: (e -> e') -> Parser i e o -> Parser i e' o
rethrow f (Parser p) = Parser (t . p)
    where
        t (Left l) = failure (map f (errors l)) (input l) (position l)
        t (Right r) = Right r

-- | @`alternatives` ps@ tries each parser in @ps@ until the first successful parse.
alternatives :: [Parser i e o] -> Parser i e o
alternatives = asum

-- | @`optional` p@ applies the parser @p@ and always succeeds, with the output `Nothing` if @p@ fails, otherwise `Just` the output of @p@.
optional :: Parser i e o -> Parser i e (Maybe o)
optional (Parser p) = Parser (p' . p)
    where
        p' (Left l) = success Nothing (input l) 0
        p' (Right r) = success (Just (output r)) (remaining r) (offset r)

-- | @`times` n p@ applies the parser @p@ @n@ times.
times :: Int -> Parser i e o -> Parser i e [o]
times 0 p = return []
times n p = do
    x <- p
    xs <- times (n - 1) p
    return (x : xs)

-- | @`many` p@ applies the parser @p@ one or more times.
many :: Parser i e o -> Parser i e [o]
many p = do
    x <- p
    xs <- many p <|> return []
    return (x : xs)

-- | @`fallback` o p@ tries the parser @p@ with its output on success, otherwise uses the output @o@.
fallback :: o -> Parser i e o -> Parser i e o
fallback o p = p <|> return o

-- | @`delimited` pd p@ applies the parser @p@ one or more times, delimited by the parser @pd@.
delimited :: Parser i e o' -> Parser i e o -> Parser i e [o]
delimited pd p = do
    x <- p
    d <- optional pd
    case d of
        Nothing -> return [x]
        Just _ -> do
            xs <- delimited pd p
            return (x : xs)