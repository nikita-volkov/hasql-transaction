module Hasql.Transaction.Sessions
(
  transaction,
  transactionPreparable,
  -- * Transaction settings
  C.Mode(..),
  C.IsolationLevel(..),
)
where

import Data.Bool
import qualified Hasql.Transaction.Private.Transaction as A
import qualified Hasql.Session as B
import qualified Hasql.Transaction.Private.Model as C


-- |
-- Execute the transaction using the provided isolation level and mode.
{-# INLINE transaction #-}
transaction :: C.IsolationLevel -> C.Mode -> A.Transaction a -> B.Session a
transaction isolation mode transaction =
  A.run transaction isolation mode True

-- |
-- Execute the transaction using the provided isolation level and mode,
-- and specifying if the BEGIN, COMMIT and ABORT statements should be prepared or not.
--
-- Helps with transaction pooling due to its incompatibility with prepared statements.
{-# INLINE transactionPreparable #-}
transactionPreparable :: C.IsolationLevel -> C.Mode -> A.Transaction a -> Bool -> B.Session a
transactionPreparable isolation mode transaction preparable =
  A.run transaction isolation mode preparable