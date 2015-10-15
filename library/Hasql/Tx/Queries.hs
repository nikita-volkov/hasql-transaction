module Hasql.Tx.Queries
where

import Hasql.Tx.Prelude
import Hasql.Query
import qualified Hasql.Serialization as S
import qualified Hasql.Deserialization as D


-- * Transactions
-------------------------

data Isolation =
  ReadCommitted |
  RepeatableRead |
  Serializable 

type TransactionMode =
  (Isolation, Bool)

beginTransaction :: TransactionMode -> ParametricQuery () ()
beginTransaction (isolation, write) =
  (template, mempty, D.result D.noResult, True)
  where
    template =
      mconcat
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

commitTransaction :: ParametricQuery () ()
commitTransaction =
  ("COMMIT", mempty, D.result D.noResult, True)

abortTransaction :: ParametricQuery () ()
abortTransaction =
  ("ABORT", mempty, D.result D.noResult, True)


-- * Streaming
-------------------------

declareCursor :: ByteString -> ParametricQuery a b -> ParametricQuery a b
declareCursor name (template, serializer, deserializer, preparable) =
  (template', serializer, deserializer, False)
  where
    template' =
      "DECLARE " <> name <> " NO SCROLL CURSOR FOR " <> template

closeCursor :: ParametricQuery ByteString ()
closeCursor =
  ("CLOSE $1", S.value (S.nonNull S.bytea), D.result D.noResult, True)

fetchFromCursor :: D.Results a -> ParametricQuery (Int64, ByteString) a
fetchFromCursor deserializer =
  (template, serializer, deserializer, True)
  where
    template =
      "FETCH FORWARD $1 FROM $2"
    serializer =
      contramap fst (S.value (S.nonNull S.int8)) <>
      contramap snd (S.value (S.nonNull S.bytea))


