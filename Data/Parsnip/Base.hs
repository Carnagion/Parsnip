{- |
Module: Data.Parsnip.Base
Description: A simple monadic parser combinator library.
Description: Contains base parser functions.
Copyright: (c) Indraneel Mahendrakumar, 2022
License: MIT
Maintainer: Indraneel Mahendrakumar <indraneel.mahendrakumar@gmail.com>
Stability: experimental
Portability: portable

The @Data.Parsnip.Base@ module exposes the basic parsing functions @`throw`@, @`peek`@, and @`consume`@, which make up the foundations of other parser combinator functions.
-}
module Data.Parsnip.Base (
    consume,
    peek,
    throw,
) where

import Data.Parsnip (ParseError (..), Parser (..), failure, success)
import Data.Parsnip.Stream (TokenStream (..))

-- | @`throw` errs@ always fails with the errors @errs@, consuming no input from the stream.
throw :: [ParseError e] -> Parser e s i o
throw = Parser . failure

-- | @`peek`@ returns @`Just`@ the current input from the stream without consuming it, or @`Nothing`@ if there is no input remaining.
peek :: TokenStream s => Parser e s i (Maybe i)
peek = Parser (\ strm -> success (current strm) 0 strm)

-- | @`consume`@ always consumes input, advancing the stream once.
consume :: TokenStream s => Parser e s i ()
consume = Parser $ success () 1 . advance