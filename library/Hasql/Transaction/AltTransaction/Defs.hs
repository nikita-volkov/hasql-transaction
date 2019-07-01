module Hasql.Transaction.AltTransaction.Defs where

import Hasql.Transaction.Prelude hiding (map, retry)
import Hasql.Transaction.Requisites.Model
import Hasql.Session (Session)
import Hasql.Statement (Statement)
import qualified Hasql.Session as Session
import qualified Hasql.Transaction.Transaction.Transaction as Transaction


{-|
Composable transaction providing for automated conflict resolution
with input @i@ and output @o@. Supports alternative branching.

Mode and level is associated with the transaction,
which makes them a part of the composition as well.
In a composed transaction they become the strictest of the ones
associated with the transactions that constitute it.
-}
data AltTransaction i o =
  AltTransaction Mode Level [i -> StateT Condemnation Session o]

deriving instance Functor (AltTransaction i)

instance Applicative (AltTransaction i) where
  pure = AltTransaction Read ReadCommitted . pure . const . pure
  (<*>) = binOp $ \ lSession rSession i -> lSession i <*> rSession i

instance Alternative (AltTransaction i) where
  empty = retry
  (<|>) (AltTransaction lMode lLevel lList) (AltTransaction rMode rLevel rList) =
    AltTransaction (max lMode rMode) (max lLevel rLevel) (lList <> rList)

instance Profunctor AltTransaction where
  dimap fn1 fn2 = map $ \ session -> fmap fn2 . session . fn1

instance Strong AltTransaction where
  first' = first
  second' = second

instance Choice AltTransaction where
  left' = left
  right' = right

instance Semigroupoid AltTransaction where
  o = binOp (<=<)

instance Category AltTransaction where
  id = AltTransaction Read ReadCommitted []
  (.) = o

instance Arrow AltTransaction where
  arr fn = AltTransaction Read ReadCommitted (return (return . fn))
  (***) = binOp $ \ lSession rSession (li, ri) -> (,) <$> lSession li <*> rSession ri

instance ArrowChoice AltTransaction where
  (+++) = binOp $ \ lSession rSession -> either (fmap Left . lSession) (fmap Right . rSession)

instance ArrowZero AltTransaction where
  zeroArrow = empty

instance ArrowPlus AltTransaction where
  (<+>) = (<|>)

{-|
Because mode and isolation are always composed the same way,
we can focus on composing just the sessions.
-}
{-# INLINE binOp #-}
binOp ::
  (
    (li -> StateT Condemnation Session lo) ->
    (ri -> StateT Condemnation Session ro) ->
    (i -> StateT Condemnation Session o)
  ) ->
  AltTransaction li lo -> AltTransaction ri ro -> AltTransaction i o
binOp composeSessions (AltTransaction lMode lLevel lList) (AltTransaction rMode rLevel rList) = let
  mode = max lMode rMode
  level = max lLevel rLevel
  list = composeSessions <$> lList <*> rList
  in AltTransaction mode level list

{-# INLINE map #-}
map ::
  ((i1 -> StateT Condemnation Session o1) -> (i2 -> StateT Condemnation Session o2)) ->
  AltTransaction i1 o1 -> AltTransaction i2 o2
map sessionFn (AltTransaction mode isolation list) = AltTransaction mode isolation (fmap sessionFn list)

{-|
Cause transaction to eventually roll back.

This allows to perform some transactional actions,
collecting their results, and decide,
whether to commit the introduced changes to the DB
based on those results,
as well as emit those results outside of the transaction.
-}
condemn :: AltTransaction () ()
condemn = AltTransaction Read ReadCommitted [\ _ -> put Condemned]

{-|
Execute a possibly multistatement SQL string under a mode and level.
SQL strings cannot be dynamically parameterized or produce a result.

__Warning:__ SQL must not be transaction-related
like `BEGIN`, `COMMIT` or `ABORT`, otherwise you'll break the abstraction.
-}
sql :: Mode -> Level -> ByteString -> AltTransaction () ()
sql mode level sql = session mode level $ \ _ -> Session.sql sql

{-|
Execute a single statement under a mode and level.

__Warning:__ The statement must not be transaction-related
like `BEGIN`, `COMMIT` or `ABORT`, otherwise you'll break the abstraction.
-}
statement :: Mode -> Level -> Statement i o -> AltTransaction i o
statement mode level statement = session mode level $ \ i -> Session.statement i statement

{-|
Execute a composition of statements under the same mode and level.

__Warning:__

1. You must know that it is possible to break the abstraction,
if you execute statements such as `BEGIN` inside of the session.

1. For the same reason you cannot execute other transactions inside of that session.

1. You must beware that in case of conflicts any IO code that you may lift
into session will get executed multiple times.
This is the way the automatic conflict resolution works.
So be cautious about doing any mutations or rocket launches in that IO!
Simply pinging for things such as current time is totally fine though.

Due to the mentioned it's highly advised to keep all the session code
inside of the definition of a transaction.
Thus you'll be guaranteed to have control over what's going on inside of the
executed session and it will not be possible for this code
to be affected by any outside changes or used elsewhere.
-}
session :: Mode -> Level -> (i -> Session o) -> AltTransaction i o
session mode level sessionFn = AltTransaction mode level [lift . sessionFn]

{-|
Lift a non-alternating arrow-transaction.
-}
transaction :: Transaction.Transaction i o -> AltTransaction i o
transaction (Transaction.Transaction mode level sessionFn) = AltTransaction mode level [sessionFn]

{-|
Fail the alternation branch, retrying with the other. Same as `empty`.

Beware that if all your alternatives end up being a retry,
you'll get yourself a perfect infinite loop.
-}
retry :: AltTransaction i o
retry = AltTransaction Read ReadCommitted []
