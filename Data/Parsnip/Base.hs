module Data.Parsnip.Base (
    consume,
    peek,
    throw,
) where

import Data.Parsnip (ParseError (..), Parser (..), failure, success)
import Data.Parsnip.Stream (TokenStream (..))

throw :: [ParseError e] -> Parser e s i o
throw = Parser . failure

peek :: TokenStream s => Parser e s i (Maybe i)
peek = Parser (\ strm -> success (current strm) 0 strm)

consume :: TokenStream s => Parser e s i ()
consume = Parser $ success () 1 . advance