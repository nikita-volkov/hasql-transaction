module Hasql.Transaction.Transaction
(
  {-|
  Arrow DSL for composition of transactions with automated conflict resolution.
  -}
  {-* Definitions -}
  Transaction,
  statement,
  sql,
  session,
  condemn,
  retry,
  {-* Settings -}
  Mode(..),
  Level(..),
  {-* Reexports -}
  {-|
  This module also reexports the following types for you to require lesser imports:
  
  * `Reexports.Session`
  * `Reexports.Statement`
  -}
  {-* FAQ -}
  {-|
  == Why does transaction have to be an arrow, why is monad or applicative not enough?

  Arrow allows us to determine the read mode and isolation level,
  while composing transactions in such a way that one can depend on the result of the other.
  
  A monadic interface wouldn't allow us to do the first, namely:
  to compose the modes and levels. 
  
  An applicative interface wouldn't allow the second:
  to make a transaction depend on the result of the other.
  For details see the docs on the `Transaction` type.
  -}
)
where

import Hasql.Transaction.Requisites.Model
import Hasql.Transaction.Transaction.Defs
import qualified Hasql.Session as Reexports (Session)
import qualified Hasql.Statement as Reexports (Statement)
