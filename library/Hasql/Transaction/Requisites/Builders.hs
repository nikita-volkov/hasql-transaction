module Hasql.Transaction.Requisites.Builders
where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Requisites.Model
import ByteString.StrictBuilder


beginTransaction :: Mode -> IsolationLevel -> Builder
beginTransaction mode_ isolationLevel_ =
  "begin " <> isolationLevel isolationLevel_ <> " " <> mode mode_

isolationLevel :: IsolationLevel -> Builder
isolationLevel = \ case
  ReadCommitted -> "isolation level read committed"
  RepeatableRead -> "isolation level repeatable read"
  Serializable -> "isolation level serializable"

mode :: Mode -> Builder
mode = \ case
  Write -> "read write"
  Read -> "read only"