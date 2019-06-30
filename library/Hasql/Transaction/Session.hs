{-|
Extensions to Session.
Can be used imported in the same namespace as Hasql.Session.
-}
module Hasql.Transaction.Session where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Requisites.Model
import Hasql.Session
import qualified Hasql.Transaction.Transaction.Transaction as Transaction
import qualified Hasql.Transaction.Requisites.Sessions as Sessions


transaction :: i -> Transaction.Transaction i o -> Session (Maybe o)
transaction i (Transaction.Transaction mode level list) =
  Sessions.inRetryingTransaction mode level (fmap (\ fn -> runStateT (fn i) Uncondemned) list)
