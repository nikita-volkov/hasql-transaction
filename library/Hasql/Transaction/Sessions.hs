module Hasql.Transaction.Sessions
(
  transaction,
  -- * Transaction settings
  C.Mode(..),
  C.IsolationLevel(..),
)
where

import qualified Hasql.Transaction.Private.Transaction as A
import qualified Hasql.Session as B
import qualified Hasql.Transaction.Private.Model as C


-- |
-- Execute the transaction using the provided isolation level and mode.
{-# INLINE transaction #-}
transaction :: C.IsolationLevel -> C.Mode -> A.Transaction a -> B.Session a
transaction isolation mode transaction =
  A.run transaction isolation mode
