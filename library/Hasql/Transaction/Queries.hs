module Hasql.Transaction.Queries
where

import Hasql.Transaction.Prelude
import qualified Hasql.Query as HQ
import qualified Hasql.Encoders as HE
import qualified Hasql.Decoders as HD
import qualified ByteString.TreeBuilder as TB


-- * Transactions
-------------------------

data Isolation =
  ReadCommitted |
  RepeatableRead |
  Serializable 

type TransactionMode =
  (Isolation, Bool)

beginTransaction :: TransactionMode -> HQ.Query () ()
beginTransaction (isolation, write) =
  HQ.statement sql HE.unit HD.unit True
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

commitTransaction :: HQ.Query () ()
commitTransaction =
  HQ.statement "commit" HE.unit HD.unit True

abortTransaction :: HQ.Query () ()
abortTransaction =
  HQ.statement "abort" HE.unit HD.unit True


-- * Streaming
-------------------------

declareCursor :: ByteString -> ByteString -> HE.Params a -> HQ.Query a ()
declareCursor name sql encoder =
  HQ.statement sql' encoder HD.unit False
  where
    sql' =
      TB.toByteString $
      "DECLARE " <> TB.byteString name <> " NO SCROLL CURSOR FOR " <> TB.byteString sql

closeCursor :: HQ.Query ByteString ()
closeCursor =
  HQ.statement "CLOSE $1" (HE.value HE.bytea) HD.unit True

fetchFromCursor :: (b -> a -> b) -> b -> HD.Row a -> HQ.Query (Int64, ByteString) b
fetchFromCursor step init rowDec =
  HQ.statement sql encoder decoder True
  where
    sql =
      "FETCH FORWARD $1 FROM $2"
    encoder =
      contrazip2
        (HE.value HE.int8)
        (HE.value HE.bytea)
    decoder =
      HD.foldlRows step init rowDec
