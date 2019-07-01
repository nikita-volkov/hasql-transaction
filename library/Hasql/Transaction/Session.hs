{-|
Extensions to Session.
Can be used imported in the same namespace as @Hasql.Session@ without conflicts.
-}
module Hasql.Transaction.Session
(
  altTransaction,
  {-* Reexports of non-conflicting types -}
  {-|
  Despite them not being automatically rendered in the docs here we also reexport
  the following types for you to require lesser imports:
  
  * `Reexports.AltTransaction`
  -}
  module Reexports,
)
where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Requisites.Model
import Hasql.Transaction.Requisites.Sessions
import Hasql.Session
import qualified Hasql.Transaction.AltTransaction.Defs as AltTransaction
import qualified Hasql.Transaction.AltTransaction.Defs as Reexports (AltTransaction)


{-|
Execute an alternating transaction arrow providing an input for it.
-}
altTransaction :: i -> AltTransaction.AltTransaction i o -> Session o
altTransaction i (AltTransaction.AltTransaction mode level list) =
  inAlternatingTransaction mode level (fmap (\ fn -> runStateT (fn i) Uncondemned) list)
