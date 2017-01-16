module Hasql.Transaction.Private.Transaction
where

import Hasql.Transaction.Private.Prelude
import Hasql.Transaction.Private.Model
import qualified Hasql.Query as A
import qualified Hasql.Session as B
import qualified Hasql.Transaction.Private.Queries as C
import qualified Hasql.Transaction.Private.Sessions as D


-- |
-- A composable abstraction over the retryable transactions.
--
-- Executes multiple queries under the specified mode and isolation level,
-- while automatically retrying the transaction in case of conflicts.
-- Thus this abstraction closely reproduces the behaviour of 'STM'.
newtype Transaction a =
  Transaction (StateT Bool B.Session a)
  deriving (Functor, Applicative, Monad)

-- |
-- Execute the transaction using the provided isolation level and mode.
{-# INLINE run #-}
run :: Transaction a -> IsolationLevel -> Mode -> B.Session a
run (Transaction session) isolation mode =
  D.inRetryingTransaction isolation mode (runStateT session True)

-- |
-- Possibly a multi-statement query,
-- which however cannot be parameterized or prepared,
-- nor can any results of it be collected.
{-# INLINE sql #-}
sql :: ByteString -> Transaction ()
sql =
  Transaction . lift . B.sql

-- |
-- Parameters and a specification of the parametric query to apply them to.
{-# INLINE query #-}
query :: a -> A.Query a b -> Transaction b
query params query =
  Transaction . lift $ B.query params query

-- |
-- Cause transaction to eventually roll back.
{-# INLINE condemn #-}
condemn :: Transaction ()
condemn =
  Transaction $ put False
