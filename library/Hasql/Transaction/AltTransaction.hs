module Hasql.Transaction.AltTransaction
(
  {-|
  Arrow DSL for composition of transactions with automated conflict resolution.

  /Why does it have to be an arrow, why is monad or applicative not enough?/

  Arrow allows us to determine the read mode and isolation level,
  while composing transactions in such a way that one can depend on the result of the other.
  A monadic interface wouldn't allow us to do the first, namely:
  to compose the modes and levels. 
  An applicative interface wouldn't allow the second:
  to make a transaction depend on the result of the other.
  For details see the docs on the `Transaction` type.
  -}
  AltTransaction,
  statement,
  sql,
  session,
  condemn,
  transaction,
  -- * Settings
  Mode(..),
  Level(..),
)
where

import Hasql.Transaction.Requisites.Model
import Hasql.Transaction.AltTransaction.Defs
