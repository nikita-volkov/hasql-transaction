module Hasql.Transaction.Requisites.Builders
where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Requisites.Model
import ByteString.StrictBuilder


beginTransaction :: Mode -> Level -> Builder
beginTransaction mode_ level_ =
  "begin " <> level level_ <> " " <> mode mode_

level :: Level -> Builder
level = \ case
  ReadCommitted -> "isolation level read committed"
  RepeatableRead -> "isolation level repeatable read"
  Serializable -> "isolation level serializable"

mode :: Mode -> Builder
mode = \ case
  Write -> "read write"
  Read -> "read only"
