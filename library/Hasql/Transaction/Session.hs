{-|
Extensions to Session.
Can be used imported in the same namespace as Hasql.Session.
-}
module Hasql.Transaction.Session where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Requisites.Model
import Hasql.Session
import qualified Hasql.Transaction.AltTransaction.Defs as AltTransaction
import qualified Hasql.Transaction.Requisites.Sessions as Sessions


altTransaction :: i -> AltTransaction.AltTransaction i o -> Session (Maybe o)
altTransaction i (AltTransaction.AltTransaction mode level list) =
  Sessions.inRetryingTransaction mode level (fmap (\ fn -> runStateT (fn i) Uncondemned) list)
