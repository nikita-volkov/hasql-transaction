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

beginTransaction :: IsolationLevel -> Mode -> Bool -> A.Statement () ()
beginTransaction isolation mode preparable =
  A.Statement (D.beginTransaction isolation mode) B.noParams C.noResult preparable

commitTransaction :: Bool -> A.Statement () ()
commitTransaction preparable =
  A.Statement "COMMIT" B.noParams C.noResult preparable

abortTransaction :: Bool -> A.Statement () ()
abortTransaction preparable =
  A.Statement "ABORT" B.noParams C.noResult preparable


-- * Streaming
-------------------------

declareCursor :: ByteString -> ByteString -> B.Params a -> A.Statement a ()
declareCursor name sql encoder =
  A.Statement (D.declareCursor name sql) encoder C.noResult False

closeCursor :: A.Statement ByteString ()
closeCursor =
  A.Statement "CLOSE $1" ((B.param . B.nonNullable) B.bytea) C.noResult True

fetchFromCursor :: (b -> a -> b) -> b -> C.Row a -> A.Statement (Int64, ByteString) b
fetchFromCursor step init rowDec =
  A.Statement sql encoder decoder True
  where
    sql =
      "FETCH FORWARD $1 FROM $2"
    encoder =
      contrazip2
        ((B.param . B.nonNullable) B.int8)
        ((B.param . B.nonNullable) B.bytea)
    decoder =
      C.foldlRows step init rowDec
