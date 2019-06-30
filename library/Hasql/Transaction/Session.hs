{-|
Extensions to Session.
Can be used imported in the same namespace as Hasql.Session.
-}
module Hasql.Transaction.Session where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Requisites.Model
import Hasql.Transaction.Requisites.Sessions
import Hasql.Session
import qualified Hasql.Transaction.Transaction.Transaction as Transaction


transaction :: i -> Transaction.Transaction i o -> Session o
transaction input (Transaction.Transaction mode level session) =
  inRetryingTransaction mode level (runStateT (session input) Uncondemned)
