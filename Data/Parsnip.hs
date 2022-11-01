module Data.Parsnip (
    ParseError (..),
    ParseFailure (..),
    Parser (..),
    ParseSuccess (..),
    failure,
    success,
) where

import Control.Applicative (Alternative (..))

newtype Parser e s i o
    = Parser { parse :: s i -> Either (ParseFailure e s i) (ParseSuccess s i o) }

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

data ParseSuccess s i o
    = Success { output :: o, offset :: Int, remaining :: s i }

success out ofst rem = Right $ Success out ofst rem

data ParseFailure e s i
    = Failure { errors :: [ParseError e], input :: s i }

failure errs inp = Left $ Failure errs inp

data ParseError e
    = Error { cause :: e, at :: Int }