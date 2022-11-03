{- |
Module: Data.Parsnip.Combinators
Description: A simple monadic parser combinator library.
Description: Contains common parser combinator functions.
Copyright: (c) Indraneel Mahendrakumar, 2022
License: MIT
Maintainer: Indraneel Mahendrakumar <indraneel.mahendrakumar@gmail.com>
Stability: experimental
Portability: portable

The @Data.Parsnip.Combinators@ module exposes common polymorphic parser combinator functions, building up on the basic parsers defined in @`Data.Parsnip.Base`@.
-}
module Data.Parsnip.Combinators (
    TokenError (..),
    delimit,
    fallback,
    just,
    justSeq,
    none,
    oneOf,
    oneOrMore,
    optional,
    rethrow,
    satisfy,
    some,
    times,
    zeroOrMore,
) where

import Control.Applicative (Alternative ((<|>)))

import Data.Foldable (asum)

import Data.Parsnip (ParseError (..), ParseFailure (..), Parser (..), ParseSuccess (..), failure, success)
import Data.Parsnip.Base (consume, peek, throw)
import Data.Parsnip.Stream (TokenStream (..))

-- | @`TokenError` i@ is a parser error type covering basic token consumption and matching errors.
data TokenError i
    -- | Some input was expected, but none was received.
    = ExpectedInput
    -- | No input was expected, but some was received.
    | UnexpectedInput i
    -- | Input was expected to satisfy some predicate, but it did not.
    | NonsatisfyingInput i
    -- | Input was expected to equal another value, but it did not.
    | UnequalInput i i

-- | @`some`@ succeeds on and consumes any input, failing if there is no input available.
some :: TokenStream s => Parser (TokenError i) s i i
some = do
    cur <- peek
    case cur of
        Nothing -> throw [Error ExpectedInput 0]
        Just tkn -> do
            consume
            return tkn

-- | @`none`@ succeeds on an empty input stream, failing if there is some input available.
none :: TokenStream s => Parser (TokenError i) s i ()
none = do
    cur <- peek
    case cur of
        Nothing -> return ()
        Just tkn -> throw [Error (UnexpectedInput tkn) 0]

-- | @`satisfy` f@ succeeds if the current input satisfies the predicate @f@, consuming it, and fails otherwise.
satisfy :: TokenStream s => (i -> Bool) -> Parser (TokenError i) s i i
satisfy f = do
    cur <- peek
    case cur of
        Nothing -> throw [Error ExpectedInput 0]
        Just tkn
            | f tkn -> do
                consume
                return tkn
            | otherwise -> throw [Error (NonsatisfyingInput tkn) 0]

-- | @`just` x@ succeeds if the current input equals @x@, consuming it, and fails otherwise.
--
-- It is semantically equivalent to @`satisfy` (== x)@.
just :: (Eq i, TokenStream s) => i -> Parser (TokenError i) s i i
just x = rethrow f (satisfy (== x))
    where
        f (NonsatisfyingInput inp) = UnequalInput inp x
        f err = err

-- | @`justSeq` strm@ is like @`just`@, but accepts a stream @strm@ to compare, rather than a single input token.
justSeq :: (Eq i, TokenStream s) => s i -> Parser (TokenError i) s i [i]
justSeq strm = f strm []
    where
        f strm out = case current strm of
            Nothing -> return $ reverse out
            Just tkn -> do
                x <- just tkn
                f (advance strm) (x : out)

-- | @`rethrow` f p@ maps errors in the parser @p@ to another error type using @f@.
rethrow :: TokenStream s => (e -> e') -> Parser e s i o -> Parser e' s i o
rethrow f (Parser p) = Parser $ p' . p
    where
        p' (Left l) = failure (map (\ (Error err pos) -> Error (f err) pos) (errors l)) (input l)
        p' (Right r) = Right r

-- | @`fallback` o p@ returns the output @o@ if the parser @p@ fails.
--
-- It is semantically equivalent to @p <|> return o@.
fallback :: o -> Parser e s i o -> Parser e s i o
fallback o p = p <|> return o

-- | @`optional` p@ always succeeds with @`Just`@ the output of @p@ or @`Nothing`@ if @p@ fails.
optional :: Parser e s i o -> Parser e s i (Maybe o)
optional (Parser p) = Parser (p' . p)
    where
        p' (Left l) = success Nothing 0 (input l)
        p' (Right r) = success (Just $ output r) (offset r) (remaining r)

-- | @`oneOf` ps@ tries each parser in @ps@, failing only if all of them fail.
oneOf :: [Parser e s i o] -> Parser e s i o
oneOf = asum

-- | @`times` n p@ applies the parser @p@ exactly @n@ times.
times :: Int -> Parser e s i o -> Parser e s i [o]
times 0 p = return []
times n p = do
    x <- p
    xs <- times (n - 1) p
    return $ x : xs

-- | @`zeroOrMore` p@ tries to apply the parser @p@ zero or more times.
zeroOrMore :: Parser e s i o -> Parser e s i [o]
zeroOrMore = fallback [] . oneOrMore

-- | @`oneOrMore` p@ applies the parser @p@ at least once, and then tries to apply it zero or more times.
oneOrMore :: Parser e s i o -> Parser e s i [o]
oneOrMore p = do
    x <- p
    xs <- zeroOrMore p
    return $ x : xs

-- | @`delimit` pd p@ applies the parser @p@ one or more times, delimited by the parser @pd@.
delimit :: Parser e s i o' -> Parser e s i o -> Parser e s i [o]
delimit pd p = do
    x <- p
    d <- optional pd
    case d of
        Nothing -> return [x]
        Just _ -> do
            xs <- delimit pd p
            return $ x : xs