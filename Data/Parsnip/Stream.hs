{- |
Module: Data.Parsnip.Stream
Description: Contains the @`TokenStream`@ typeclass and its various instances.
Copyright: (c) Indraneel Mahendrakumar, 2022
License: MIT
Maintainer: Indraneel Mahendrakumar <indraneel.mahendrakumar@gmail.com>
Stability: experimental
Portability: portable

The @Data.Parsnip.Stream@ module exposes the @`TokenStream`@ typeclass and implements various instances of it.
-}
module Data.Parsnip.Stream (
    TokenStream (..),
) where

-- | @`TokenStream`@ represents a stream of input tokens that can be consumed by parsers.
class TokenStream s where
    -- | @`advance` strm@ advances the stream @strm@ by one token.
    advance :: s i -> s i
    -- | @`current` strm@ retrieves @`Just`@ the current token of the stream @strm@, or @`Nothing`@ if it is empty.
    current :: s i -> Maybe i

instance TokenStream [] where
    advance [] = []
    advance (_ : xs) = xs

    current [] = Nothing
    current (x : _) = Just x