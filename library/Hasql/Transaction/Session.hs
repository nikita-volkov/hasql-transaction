{-|
Extensions to Session.
Can be used imported in the same namespace as Hasql.Session.
-}
module Hasql.Transaction.Session where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Transaction.Transaction
import Hasql.Session


transaction :: i -> Transaction i o -> Session o
transaction = undefined
