module Hasql.Transaction.Statements
where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Model
import Hasql.Statement
import qualified Hasql.Encoders as B
import qualified Hasql.Decoders as C
import qualified Hasql.Transaction.Sql as D


-- * Transactions
-------------------------

beginTransaction :: IsolationLevel -> Mode -> Statement () ()
beginTransaction isolation mode =
  Statement (D.beginTransaction isolation mode) B.noParams C.noResult True

commitTransaction :: Statement () ()
commitTransaction =
  Statement "COMMIT" B.noParams C.noResult True

abortTransaction :: Statement () ()
abortTransaction =
  Statement "ABORT" B.noParams C.noResult True


-- * Streaming
-------------------------

declareCursor :: ByteString -> ByteString -> B.Params a -> Statement a ()
declareCursor name sql encoder =
  Statement (D.declareCursor name sql) encoder C.noResult False

closeCursor :: Statement ByteString ()
closeCursor =
  Statement "CLOSE $1" ((B.param . B.nonNullable) B.bytea) C.noResult True

fetchFromCursor :: (b -> a -> b) -> b -> C.Row a -> Statement (Int64, ByteString) b
fetchFromCursor step init rowDec =
  Statement sql encoder decoder True
  where
    sql =
      "FETCH FORWARD $1 FROM $2"
    encoder =
      contrazip2
        ((B.param . B.nonNullable) B.int8)
        ((B.param . B.nonNullable) B.bytea)
    decoder =
      C.foldlRows step init rowDec
