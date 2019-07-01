{-|
Extensions to Session.
Can be used imported in the same namespace as @Hasql.Session@ without conflicts.
-}
module Hasql.Transaction.Session
(
  transaction,
  {-* Reexports of non-conflicting types -}
  {-|
  Despite them not being rendered in the docs here we also reexport
  the following types for you to specify lesser imports:
  
  * `Reexports.Transaction`
  -}
  module Reexports,
)
where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Requisites.Model
import Hasql.Transaction.Requisites.Sessions
import Hasql.Session
import qualified Hasql.Transaction.Transaction.Transaction as Transaction
import qualified Hasql.Transaction.Transaction.Transaction as Reexports (Transaction)


{-|
Execute a transaction arrow providing an input for it.
-}
transaction :: i -> Transaction.Transaction i o -> Session o
transaction input (Transaction.Transaction mode level session) =
  inRetryingTransaction mode level (runStateT (session input) Uncondemned)
