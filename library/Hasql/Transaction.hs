-- |
-- An API for declaration of transactions.
module Hasql.Transaction
(
  -- * Transaction settings
  Mode(..),
  IsolationLevel(..),
  -- * Transaction monad
  Transaction,
  condemn,
  sql,
  query,
)
where

import Hasql.Transaction.Private.Transaction
import Hasql.Transaction.Private.Model
