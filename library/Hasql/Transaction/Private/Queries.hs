module Hasql.Transaction.Private.Queries
where

import Hasql.Transaction.Private.Prelude
import qualified Hasql.Query as A
import qualified Hasql.Encoders as B
import qualified Hasql.Decoders as C
import qualified ByteString.TreeBuilder as D


-- * Transactions
-------------------------

data Isolation =
  ReadCommitted |
  RepeatableRead |
  Serializable 

type TransactionMode =
  (Isolation, Bool)

beginTransaction :: TransactionMode -> A.Query () ()
beginTransaction (isolation, write) =
  A.statement sql B.unit C.unit True
  where
    sql =
      D.toByteString $
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

commitTransaction :: A.Query () ()
commitTransaction =
  A.statement "commit" B.unit C.unit True

abortTransaction :: A.Query () ()
abortTransaction =
  A.statement "abort" B.unit C.unit True


-- * Streaming
-------------------------

declareCursor :: ByteString -> ByteString -> B.Params a -> A.Query a ()
declareCursor name sql encoder =
  A.statement sql' encoder C.unit False
  where
    sql' =
      D.toByteString $
      "DECLARE " <> D.byteString name <> " NO SCROLL CURSOR FOR " <> D.byteString sql

closeCursor :: A.Query ByteString ()
closeCursor =
  A.statement "CLOSE $1" (B.value B.bytea) C.unit True

fetchFromCursor :: (b -> a -> b) -> b -> C.Row a -> A.Query (Int64, ByteString) b
fetchFromCursor step init rowDec =
  A.statement sql encoder decoder True
  where
    sql =
      "FETCH FORWARD $1 FROM $2"
    encoder =
      contrazip2
        (B.value B.int8)
        (B.value B.bytea)
    decoder =
      C.foldlRows step init rowDec
