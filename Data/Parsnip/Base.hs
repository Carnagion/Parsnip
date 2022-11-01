module Data.Parsnip.Base (
    ConsumeError (..),
    just,
    justSeq,
    none,
    satisfy,
    some,
) where

import Data.Foldable (asum)

import Data.Parsnip (ParseError (..), Parser (..), failure, success)
import Data.Parsnip.Combinators (rethrow)
import Data.Parsnip.Stream (TokenStream (..))

data ConsumeError i
    = ExpectedInput
    | UnexpectedInput i
    | NonsatisfyingInput i
    | UnequalInput i i

some :: TokenStream s => Parser (ConsumeError i) s i i
some = Parser p
    where
        p strm = case current strm of
            Nothing -> failure [Error ExpectedInput 0] strm
            Just tkn -> success tkn 1 (advance strm)

none :: TokenStream s => Parser (ConsumeError i) s i ()
none = Parser p
    where
        p strm = case current strm of
            Nothing -> success () 0 strm
            Just tkn -> failure [Error (UnexpectedInput tkn) 0] strm

satisfy :: TokenStream s => (i -> Bool) -> Parser (ConsumeError i) s i i
satisfy f = Parser p'
    where
        p' strm = case current strm of
            Nothing -> failure [Error ExpectedInput 0] strm
            Just tkn
                | f tkn -> success tkn 1 (advance strm)
                | otherwise -> failure [Error (NonsatisfyingInput tkn) 0] strm

just :: (Eq i, TokenStream s) => i -> Parser (ConsumeError i) s i i
just x = rethrow f (satisfy (== x))
    where
        f (NonsatisfyingInput inp) = UnequalInput inp x
        f err = err

justSeq :: (Eq i, TokenStream s) => s i -> Parser (ConsumeError i) s i [i]
justSeq strm = f strm []
    where
        f strm out = case current strm of
            Nothing -> return $ reverse out
            Just tkn -> do
                x <- just tkn
                f (advance strm) (x : out)