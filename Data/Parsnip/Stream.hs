module Data.Parsnip.Stream (
    TokenStream (..),
) where

class TokenStream s where
    advance :: s i -> s i
    current :: s i -> Maybe i

instance TokenStream [] where
    advance [] = []
    advance (_ : xs) = xs

    current [] = Nothing
    current (x : _) = Just x