module Hasql.Transaction.Requisites.Model
where

import Hasql.Transaction.Prelude


{-|
Execution mode: either read or write.
-}
data Mode =
  {-|
  Read-only. No writes possible.
  -}
  Read |
  {-|
  Write and commit.
  -}
  Write
  deriving (Show, Eq, Ord, Enum, Bounded)

{-|
Transaction isolation level.

For reference see
<http://www.postgresql.org/docs/current/static/transaction-iso.html the Postgres' documentation>.
-}
data Level =
  ReadCommitted |
  RepeatableRead |
  Serializable
  deriving (Show, Eq, Ord, Enum, Bounded)

data Condemnation =
  Condemned |
  Uncondemned
  deriving (Show, Eq, Ord, Enum, Bounded)
