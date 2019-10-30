module Hasql.Transaction.Requisites.Statements
where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Requisites.Types
import Hasql.Statement
import qualified ByteString.StrictBuilder as Builder
import qualified Hasql.Encoders as B
import qualified Hasql.Decoders as C
import qualified Hasql.Transaction.Requisites.Builders as Builders


beginTransaction :: Mode -> Level -> Statement () ()
beginTransaction mode level =
  Statement (Builder.builderBytes (Builders.beginTransaction mode level)) B.noParams C.noResult True

commitTransaction :: Statement () ()
commitTransaction =
  Statement "commit" B.noParams C.noResult True

abortTransaction :: Statement () ()
abortTransaction =
  Statement "abort" B.noParams C.noResult True
