{- |
Module: Data.Parsnip
Description: Contains core parser types.
Copyright: (c) Indraneel Mahendrakumar, 2022
License: MIT
Maintainer: Indraneel Mahendrakumar <indraneel.mahendrakumar@gmail.com>
Stability: experimental
Portability: portable

The @Data.Parsnip@ module exposes the core parsing types @`Parser`@, @`ParseSuccess`@, @`ParseFailure`@, and @`ParseError`@.
-}
module Data.Parsnip (
    ParseError (..),
    ParseFailure (..),
    Parser (..),
    ParseSuccess (..),
    failure,
    success,
) where

import Control.Applicative (Alternative (..))

-- | @`Parser` e s i o@ is a parser that accepts an input stream of type @s i@, and produces either errors of type @e@ or output of type @o@.
newtype Parser e s i o = Parser
    { -- | Parses an input stream of type @s i@ and returns @`Either`@ a @`ParseFailure` e s i@ or a @`ParseSuccess` s i o@.
      parse :: s i -> Either (ParseFailure e s i) (ParseSuccess s i o)
    }

instance Functor (Parser e s i) where
    fmap f (Parser p) = Parser (p' . p)
        where
            p' (Left l) = Left l
            p' (Right r) = success (f (output r)) (offset r) (remaining r)

instance Applicative (Parser e s i) where
    pure out = Parser (success out 0)

    Parser pl <*> Parser pr = Parser p'
        where
            p' strm = do
                xl <- pl strm
                xr <- pr $ remaining xl
                pure $ Success (output xl (output xr)) (offset xl + offset xr) (remaining xr)

instance Monad (Parser e s i) where
    return = pure

    Parser p >>= f = Parser p'
        where
            p' strm = do
                x <- p strm
                case parse (f $ output x) (remaining x) of
                    Left l -> failure (map (\ (Error err pos) -> Error err (offset x + pos)) (errors l)) (input l)
                    Right r -> success (output r) (offset x + offset r) (remaining r)

instance Alternative (Parser e s i) where
    empty = Parser (failure [])

    Parser pl <|> Parser pr = Parser p
        where
            p strm = case pl strm of
                Left l -> case pr strm of
                    Left l' -> failure (errors l ++ errors l') strm
                    Right r' -> Right r'
                Right r -> Right r

-- | @`ParseSuccess` s i o@ represents a successful parse with output of type @o@ and remaining input stream of type @s i@.
data ParseSuccess s i o = Success
    { -- | Parsed output.
      output :: o,
      -- | Amount of input that was consumed.
      offset :: Int,
      -- | Remaining input to parse.
      remaining :: s i
    }

-- | @`success` out ofst@ returns a @`ParseSuccess`@ as a @`Right`@ value.
success :: o -> Int -> s i -> Either a (ParseSuccess s i o)
success out ofst rem = Right $ Success out ofst rem

-- | @`ParseFailure` e s i@ represents a failed parse with a list of errors of type @e@ and invalid input stream of type @s i@.
data ParseFailure e s i = Failure
    { -- | List of parse errors.
      errors :: [ParseError e],
      -- | Input that caused the parsing to fail.
      input :: s i
    }

-- | @`failure` errs inp@ returns a @`ParseFailure`@ as a @`Left`@ value.
failure :: [ParseError e] -> s i -> Either (ParseFailure e s i) a
failure errs inp = Left $ Failure errs inp

-- | @`ParseError` e@ represents a parse error at a specified position and with underlying error of type @e@.
data ParseError e = Error
    { -- | Underlying cause of error.
      cause :: e,
      -- | Position where error occurred.
      at :: Int
    }