module Hasql.Transaction.Sessions
  ( transaction,
    unpreparedTransaction,

    -- * Transaction settings
    C.Mode (..),
    C.IsolationLevel (..),
  )
where

import Data.Bool
import Hasql.Session qualified as B
import Hasql.Transaction.Config qualified as C
import Hasql.Transaction.Private.Transaction qualified as A

-- |
-- Execute the transaction using the provided isolation level and mode.
{-# INLINE transaction #-}
transaction :: C.IsolationLevel -> C.Mode -> A.Transaction a -> B.Session a
transaction isolation mode transaction =
  A.run transaction isolation mode True

-- |
-- Execute the transaction using the provided isolation level and mode,
-- and specifying that the generated BEGIN, COMMIT and ABORT statements should not be prepared.
--
-- Helps with transaction pooling due to its incompatibility with prepared statements.
{-# INLINE unpreparedTransaction #-}
unpreparedTransaction :: C.IsolationLevel -> C.Mode -> A.Transaction a -> B.Session a
unpreparedTransaction isolation mode transaction =
  A.run transaction isolation mode False
