{-|
Extensions to Session.
Can be used imported in the same namespace as Hasql.Session.
-}
module Hasql.Transaction.Session where

import Hasql.Transaction.Prelude
import Hasql.Session
import qualified Hasql.Transaction.Transaction.Transaction as Transaction
import qualified Hasql.Transaction.Requisites.Sessions as Sessions


transaction :: i -> Transaction.Transaction i o -> Session o
transaction i (Transaction.Transaction mode level session) = undefined
