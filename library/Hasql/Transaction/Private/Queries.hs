module Hasql.Transaction.Private.Queries
where

import Hasql.Transaction.Private.Prelude
import Hasql.Transaction.Private.Model
import qualified Hasql.Query as A
import qualified Hasql.Encoders as B
import qualified Hasql.Decoders as C
import qualified Hasql.Transaction.Private.SQL as D


-- * Transactions
-------------------------

beginTransaction :: IsolationLevel -> Mode -> A.Query () ()
beginTransaction isolation mode =
  A.statement (D.beginTransaction isolation mode) B.unit C.unit True

commitTransaction :: A.Query () ()
commitTransaction =
  A.statement "COMMIT" B.unit C.unit True

abortTransaction :: A.Query () ()
abortTransaction =
  A.statement "ABORT" B.unit C.unit True


-- * Streaming
-------------------------

declareCursor :: ByteString -> ByteString -> B.Params a -> A.Query a ()
declareCursor name sql encoder =
  A.statement (D.declareCursor name sql) encoder C.unit False

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
