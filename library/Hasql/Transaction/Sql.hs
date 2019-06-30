module Hasql.Transaction.Sql
where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Model
import qualified ByteString.TreeBuilder as D


beginTransaction :: IsolationLevel -> Mode -> ByteString
beginTransaction isolation mode =
  D.toByteString builder
  where
    builder =
      "BEGIN " <> isolationBuilder <> " " <> modeBuilder
      where
        isolationBuilder =
          case isolation of
            ReadCommitted -> "ISOLATION LEVEL READ COMMITTED"
            RepeatableRead -> "ISOLATION LEVEL REPEATABLE READ"
            Serializable -> "ISOLATION LEVEL SERIALIZABLE"
        modeBuilder =
          case mode of
            Write -> "READ WRITE"
            Read -> "READ ONLY"

declareCursor :: ByteString -> ByteString -> ByteString
declareCursor name sql =
  D.toByteString $
  "DECLARE " <> D.byteString name <> " NO SCROLL CURSOR FOR " <> D.byteString sql
