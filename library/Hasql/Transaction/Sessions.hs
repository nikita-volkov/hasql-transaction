module Hasql.Transaction.Sessions
  ( transaction,

    -- * Transaction settings
    C.Mode (..),
    C.IsolationLevel (..),
  )
where

import Hasql.Session qualified as B
import Hasql.Transaction.Config qualified as C
import Hasql.Transaction.Private.Transaction qualified as A

-- |
-- Execute the transaction using the provided isolation level and mode.
{-# INLINE transaction #-}
transaction :: C.IsolationLevel -> C.Mode -> A.Transaction a -> B.Session a
transaction isolation mode transaction =
  A.run transaction isolation mode
