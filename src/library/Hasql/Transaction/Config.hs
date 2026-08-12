module Hasql.Transaction.Config where

import Hasql.Transaction.Private.Prelude

data Mode
  = -- |
    -- Read-only. No writes possible.
    Read
  | -- |
    -- Write and commit.
    Write
  deriving (Show, Eq, Ord, Enum, Bounded)

-- |
-- Combines two modes by picking the one that grants more capability.
--
-- 'mempty' is 'Read', the identity of 'max': it never overrides an
-- explicit 'Write' requirement, so a piece of a composed transaction
-- that needs to write always wins over pieces that don't care.
instance Semigroup Mode where
  (<>) = max

instance Monoid Mode where
  mempty = minBound

-- |
-- For reference see
-- <http://www.postgresql.org/docs/current/static/transaction-iso.html the Postgres' documentation>.
data IsolationLevel
  = ReadCommitted
  | RepeatableRead
  | Serializable
  deriving (Show, Eq, Ord, Enum, Bounded)

-- |
-- Combines two isolation levels by picking the stricter one.
--
-- 'mempty' is 'ReadCommitted', the identity of 'max': it never overrides
-- an explicit stricter requirement, so a piece of a composed transaction
-- that needs e.g. 'Serializable' always wins over pieces that don't care.
instance Semigroup IsolationLevel where
  (<>) = max

instance Monoid IsolationLevel where
  mempty = minBound
