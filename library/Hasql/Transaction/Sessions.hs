module Hasql.Transaction.Sessions
(
  transaction,
)
where

import qualified Hasql.Transaction.Private.Transaction as A
import qualified Hasql.Session as B


-- |
-- Execute the transaction using the provided isolation level and mode.
{-# INLINE transaction #-}
transaction :: A.IsolationLevel -> A.Mode -> A.Transaction a -> B.Session a
transaction isolation mode transaction =
  A.run transaction isolation mode
