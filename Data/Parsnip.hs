module Data.Parsnip (
    Parser (..),
    ParseSuccess (..),
    ParseFailure (..),
) where

data ParseSuccess i o = Success {output :: o, remaining :: i, offset :: Int}
    deriving (Show)

success o i n = Right (Success o i n)

data ParseFailure i e = Failure {errors :: [e], input :: i, position :: Int}
    deriving (Show)

failure e i n = Left (Failure e i n)

newtype Parser i e o = Parser {parse :: i -> Either (ParseFailure i e) (ParseSuccess i o)}

instance Functor (Parser i e) where
    fmap f (Parser p) = Parser (p' . p)
        where
            p' (Left l) = Left l
            p' (Right r) = success (f (output r)) (remaining r) (offset r)

instance Applicative (Parser i e) where
    pure o = Parser (\i -> success o i 0)

    Parser pl <*> Parser pr = Parser p'
        where
            p' i = do
                xl <- pl i
                xr <- pr (remaining xl)
                return (Success (output xl (output xr)) (remaining xr) (offset xl + offset xr))