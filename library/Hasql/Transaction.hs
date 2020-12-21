{-|
DSL for composition of transactions with automated conflict resolution.
-}
module Hasql.Transaction
(
  {-* Sessions -}
  transact,
  {-* Transaction -}
  Transaction,
  statement,
  sql,
  session,
  condemn,
  restrict,
  {-* Settings -}
  Mode(..),
  Level(..),
  {-* Reexports -}
  {-|
  This module also reexports the following types for you to require less imports:
  
  * `Reexports.Session`
  * `Reexports.Statement`
  -}
  module Reexports,
)
where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Types
import qualified Hasql.Session as Reexports (Session)
import qualified Hasql.Statement as Reexports (Statement)
import qualified Hasql.Session as Session
import qualified Hasql.Statement as Statement
import qualified Hasql.Transaction.Sessions as Sessions


{-|
Execute transaction in session.
-}
transact :: Transaction a -> Session.Session a
transact (Transaction mode level list) =
  Sessions.inAlternatingTransaction mode level $
  fmap (flip runStateT Uncondemned) list


{-|
Composable transaction providing for automated conflict resolution.

Mode and level is associated with the transaction,
which makes them participate in composition.
In a composed transaction they become the strictest of the ones
associated with the transactions that constitute it.
This allows you to safely compose transactions with different
ACID guarantees.

It cannot have an instance of Monad,
because it makes it impossible to implement composition of
mode and level associated with constituent transactions.
It still however is possible to compose transactions
in such a way that the result of a transaction is used
to decide which transaction to execute next,
thanks to the recently discovered `Selective` interface.

Supports alternative branching,
where the alternative gets executed in case of a transaction conflict.
-}
data Transaction a = Transaction Mode Level [StateT Condemnation Session.Session a]

deriving instance Functor Transaction

instance Applicative Transaction where
  pure = Transaction minBound minBound . pure . pure
  (<*>) (Transaction mode1 level1 list1) (Transaction mode2 level2 list2) =
    Transaction (max mode1 mode2) (max level1 level2) ((<*>) <$> list1 <*> list2)

instance Selective Transaction where
  select (Transaction mode1 level1 list1) (Transaction mode2 level2 list2) =
    Transaction (max mode1 mode2) (max level1 level2) (select <$> list1 <*> list2)

instance Alt Transaction where
  (<!>) (Transaction mode1 level1 list1) (Transaction mode2 level2 list2) =
    Transaction (max mode1 mode2) (max level1 level2) (list1 <> list2)

{-|
Execute a single statement under mode and level.

__Warning:__ The statement must not be transaction-related
like @BEGIN@, @COMMIT@ or @ABORT@, otherwise you'll break the abstraction.
-}
statement :: Mode -> Level -> Statement.Statement i o -> i -> Transaction o
statement mode level statement i = session mode level $ Session.statement i statement

{-|
Execute a possibly multistatement SQL string under a mode and level.
SQL strings cannot be dynamically parameterized or produce a result.

__Warning:__ SQL must not be transaction-related
like @BEGIN@, @COMMIT@ or @ABORT@, otherwise you'll break the abstraction.
-}
sql :: Mode -> Level -> ByteString -> Transaction ()
sql mode level sql = session mode level $ Session.sql sql

{-|
Execute a composition of statements under the same mode and level.

__Warning:__

1. You must know that it is possible to break the abstraction,
if you execute statements such as @BEGIN@ inside of the session.

1. For the same reason you cannot execute other transactions inside of that session.

1. You must beware that in case of conflicts any IO code that you may lift
into session will get executed multiple times.
This is the way the automatic conflict resolution works:
the transaction gets retried, when a conflict arises.
So be cautious about doing any mutations or rocket launches in that IO!
Simply pinging for things such as current time is totally fine though.
Still it's not recommended because it's often a symptom of bad application design.

Due to the mentioned it's highly advised to keep all the session code
inside of the definition of a transaction.
Thus you'll be guaranteed to have control over what's going on inside of the
executed session and it will not be possible for this code
to be affected by any outside changes or used elsewhere.
-}
session :: Mode -> Level -> Session.Session a -> Transaction a
session mode level session = Transaction mode level [lift session]

{-|
Cause transaction to eventually roll back.

This allows to perform some transactional actions,
collecting their results, and decide,
whether to commit the introduced changes to the DB
based on those results,
as well as emit those results outside of the transaction.
-}
condemn :: Transaction ()
condemn = Transaction minBound minBound [put Condemned]

{-|
Restrict the transaction isolation level.

Will pick up the strictest level among the one you've specified and
the one already associated with the transaction being updated.
-}
restrict :: Level -> Transaction a -> Transaction a
restrict newLevel (Transaction mode oldLevel sessions) = Transaction mode (max newLevel oldLevel) sessions
