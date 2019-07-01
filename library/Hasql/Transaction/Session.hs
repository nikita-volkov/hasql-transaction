{-|
Extensions to Session.
Can be used imported in the same namespace as @Hasql.Session@ without conflicts.
-}
module Hasql.Transaction.Session
(
  transaction,
  {-* Reexports of non-conflicting types -}
  {-|
  Despite them not being automatically rendered in the docs here we also reexport
  the following types for you to require lesser imports:
  
  * `Reexports.Transaction`
  -}
  module Reexports,
)
where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Requisites.Model
import Hasql.Transaction.Requisites.Sessions
import Hasql.Session
import qualified Hasql.Transaction.Transaction.Defs as Transaction
import qualified Hasql.Transaction.Transaction.Defs as Reexports (Transaction)


{-|
Execute an alternating transaction arrow providing an input for it.
-}
transaction :: i -> Transaction.Transaction i o -> Session o
transaction i (Transaction.Transaction mode level list) =
  inAlternatingTransaction mode level (fmap (\ fn -> runStateT (fn i) Uncondemned) list)
