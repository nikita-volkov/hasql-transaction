module Hasql.Transaction.Private.Statements
where

import Hasql.Transaction.Private.Prelude
import Hasql.Transaction.Private.Model
import qualified Hasql.Statement as A
import qualified Hasql.Encoders as B
import qualified Hasql.Decoders as C
import qualified Hasql.Transaction.Private.SQL as D


-- * Transactions
-------------------------

beginTransaction :: IsolationLevel -> Mode -> A.Statement () ()
beginTransaction isolation mode =
  A.Statement (D.beginTransaction isolation mode) B.unit C.unit True

commitTransaction :: A.Statement () ()
commitTransaction =
  A.Statement "COMMIT" B.unit C.unit True

abortTransaction :: A.Statement () ()
abortTransaction =
  A.Statement "ABORT" B.unit C.unit True


-- * Streaming
-------------------------

declareCursor :: ByteString -> ByteString -> B.Params a -> A.Statement a ()
declareCursor name sql encoder =
  A.Statement (D.declareCursor name sql) encoder C.unit False

closeCursor :: A.Statement ByteString ()
closeCursor =
  A.Statement "CLOSE $1" (B.param B.bytea) C.unit True

fetchFromCursor :: (b -> a -> b) -> b -> C.Row a -> A.Statement (Int64, ByteString) b
fetchFromCursor step init rowDec =
  A.Statement sql encoder decoder True
  where
    sql =
      "FETCH FORWARD $1 FROM $2"
    encoder =
      contrazip2
        (B.param B.int8)
        (B.param B.bytea)
    decoder =
      C.foldlRows step init rowDec
