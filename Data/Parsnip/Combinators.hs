module Data.Parsnip.Combinators (
    delimit,
    fallback,
    oneOf,
    oneOrMore,
    optional,
    rethrow,
    times,
    zeroOrMore,
) where

import Control.Applicative (Alternative ((<|>)))

import Data.Foldable (asum)

import Data.Parsnip (ParseError (..), ParseFailure (..), Parser (..), ParseSuccess (..), failure, success)
import Data.Parsnip.Stream (TokenStream (..))

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