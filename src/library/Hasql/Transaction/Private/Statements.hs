module Hasql.Transaction.Private.Statements where

import Data.Text.Encoding qualified as Text
import Hasql.Decoders qualified as C
import Hasql.Encoders qualified as B
import Hasql.Statement qualified as A
import Hasql.Transaction.Config
import Hasql.Transaction.Private.SQL qualified as D

beginTransaction :: IsolationLevel -> Mode -> A.Statement () ()
beginTransaction isolation mode =
  A.preparable (Text.decodeUtf8 (D.beginTransaction isolation mode)) B.noParams C.noResult

commitTransaction :: A.Statement () ()
commitTransaction =
  A.preparable "COMMIT" B.noParams C.noResult

abortTransaction :: A.Statement () ()
abortTransaction =
  A.preparable "ABORT" B.noParams C.noResult
