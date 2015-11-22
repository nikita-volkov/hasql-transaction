module Hasql.Tx.Queries
where

import Hasql.Tx.Prelude
import qualified Hasql as H
import qualified Hasql.Encoding as HE
import qualified Hasql.Decoding as HD
import qualified ByteString.TreeBuilder as TB


-- * Transactions
-------------------------

data Isolation =
  ReadCommitted |
  RepeatableRead |
  Serializable 

type TransactionMode =
  (Isolation, Bool)

beginTransaction :: TransactionMode -> H.Query () ()
beginTransaction (isolation, write) =
  H.Query sql HE.unit HD.unit True
  where
    sql =
      TB.toByteString $
      mconcat $
      [
        "BEGIN "
        ,
        case isolation of
          ReadCommitted  -> "ISOLATION LEVEL READ COMMITTED"
          RepeatableRead -> "ISOLATION LEVEL REPEATABLE READ"
          Serializable   -> "ISOLATION LEVEL SERIALIZABLE"
        ,
        " "
        ,
        case write of
          True  -> "READ WRITE"
          False -> "READ ONLY"
      ]

commitTransaction :: H.Query () ()
commitTransaction =
  H.Query "commit" HE.unit HD.unit True

abortTransaction :: H.Query () ()
abortTransaction =
  H.Query "abort" HE.unit HD.unit True


-- * Streaming
-------------------------

declareCursor :: ByteString -> ByteString -> HE.Params a -> H.Query a ()
declareCursor name sql encoder =
  H.Query sql' encoder HD.unit False
  where
    sql' =
      TB.toByteString $
      "DECLARE " <> TB.byteString name <> " NO SCROLL CURSOR FOR " <> TB.byteString sql

closeCursor :: H.Query ByteString ()
closeCursor =
  H.Query "CLOSE $1" (HE.value HE.bytea) HD.unit True

fetchFromCursor :: (b -> a -> b) -> b -> HD.Row a -> H.Query (Int64, ByteString) b
fetchFromCursor step init rowDec =
  H.Query sql encoder decoder True
  where
    sql =
      "FETCH FORWARD $1 FROM $2"
    encoder =
      contrazip2
        (HE.value HE.int8)
        (HE.value HE.bytea)
    decoder =
      HD.foldlRows step init rowDec
