module Data.Parsnip where

data ParseSuccess i o = Success {output :: o, remaining :: i, offset :: Int}
    deriving (Show)

success o i n = Right (Success o i n)

data ParseFailure i e = Failure {errors :: [e], input :: i, position :: Int}
    deriving (Show)

failure e i n = Left (Failure e i n)

newtype Parser i e o = Parser {parse :: i -> Either (ParseFailure i e) (ParseSuccess i o)}