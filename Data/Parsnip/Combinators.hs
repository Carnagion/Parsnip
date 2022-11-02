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

data TokenError i
    = ExpectedInput
    | UnexpectedInput i
    | NonsatisfyingInput i
    | UnequalInput i i
    deriving (Show)

some :: TokenStream s => Parser (TokenError i) s i i
some = do
    cur <- peek
    case cur of
        Nothing -> throw [Error ExpectedInput 0]
        Just tkn -> do
            consume
            return tkn

none :: TokenStream s => Parser (TokenError i) s i ()
none = do
    cur <- peek
    case cur of
        Nothing -> do
            consume
            return ()
        Just tkn -> throw [Error (UnexpectedInput tkn) 0]

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

just :: (Eq i, TokenStream s) => i -> Parser (TokenError i) s i i
just x = rethrow f (satisfy (== x))
    where
        f (NonsatisfyingInput inp) = UnequalInput inp x
        f err = err

justSeq :: (Eq i, TokenStream s) => s i -> Parser (TokenError i) s i [i]
justSeq strm = f strm []
    where
        f strm out = case current strm of
            Nothing -> return $ reverse out
            Just tkn -> do
                x <- just tkn
                f (advance strm) (x : out)

rethrow :: TokenStream s => (e -> e') -> Parser e s i o -> Parser e' s i o
rethrow f (Parser p) = Parser $ p' . p
    where
        p' (Left l) = failure (map (\ (Error err pos) -> Error (f err) pos) (errors l)) (input l)
        p' (Right r) = Right r

fallback :: o -> Parser e s i o -> Parser e s i o
fallback o p = p <|> return o

optional :: Parser e s i o -> Parser e s i (Maybe o)
optional (Parser p) = Parser (p' . p)
    where
        p' (Left l) = success Nothing 0 (input l)
        p' (Right r) = success (Just $ output r) (offset r) (remaining r)

oneOf :: [Parser e s i o] -> Parser e s i o
oneOf = asum

times :: Int -> Parser e s i o -> Parser e s i [o]
times 0 p = return []
times n p = do
    x <- p
    xs <- times (n - 1) p
    return $ x : xs

zeroOrMore :: Parser e s i o -> Parser e s i [o]
zeroOrMore = fallback [] . oneOrMore

oneOrMore :: Parser e s i o -> Parser e s i [o]
oneOrMore p = do
    x <- p
    xs <- zeroOrMore p
    return $ x : xs

delimit :: Parser e s i o' -> Parser e s i o -> Parser e s i [o]
delimit pd p = do
    x <- p
    d <- optional pd
    case d of
        Nothing -> return [x]
        Just _ -> do
            xs <- delimit pd p
            return $ x : xs