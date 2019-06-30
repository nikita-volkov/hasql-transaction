module Hasql.Transaction.Transaction.Transaction where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Requisites.Model
import Hasql.Session (Session)
import Hasql.Statement (Statement)
import qualified Hasql.Session as Session


{-|
Transaction with input @i@ and output @o@.

Why is Monad not enough for Transaction, why does it have to be an Arrow?

Arrow allows us to determine the read mode and isolation level,
while composing transactions in such a way that one can depend on the result of the other.
A monadic interface wouldn't allow us to do the first.
-}
data Transaction i o =
  Transaction Mode Level (i -> StateT Condemnation Session o)

deriving instance Functor (Transaction i)

instance Applicative (Transaction i) where
  pure = Transaction Read ReadCommitted . const . pure
  (<*>) = binOp $ \ lSession rSession i -> lSession i <*> rSession i

instance Profunctor Transaction where
  dimap fn1 fn2 = mapSession $ \ session -> fmap fn2 . session . fn1

instance Strong Transaction where
  first' = first
  second' = second

instance Choice Transaction where
  left' = left
  right' = right

instance Semigroupoid Transaction where
  o = binOp (<=<)

instance Category Transaction where
  id = Transaction Read ReadCommitted return
  (.) = o

instance Arrow Transaction where
  arr fn = Transaction Read ReadCommitted (return . fn)
  (***) = binOp $ \ lSession rSession (li, ri) -> (,) <$> lSession li <*> rSession ri

instance ArrowChoice Transaction where
  (+++) = binOp $ \ lSession rSession -> either (fmap Left . lSession) (fmap Right . rSession)

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
  Transaction li lo -> Transaction ri ro -> Transaction i o
binOp composeSessions (Transaction lMode lIsolation lSession) (Transaction rMode rIsolation rSession) = let
  mode = max lMode rMode
  isolation = max lIsolation rIsolation
  session = composeSessions lSession rSession
  in Transaction mode isolation session

{-# INLINE mapSession #-}
mapSession ::
  ((i1 -> StateT Condemnation Session o1) -> (i2 -> StateT Condemnation Session o2)) ->
  Transaction i1 o1 -> Transaction i2 o2
mapSession sessionFn (Transaction mode isolation session) = Transaction mode isolation (sessionFn session)

{-|
Cause transaction to eventually roll back.
-}
condemn :: Transaction () ()
condemn = Transaction Read ReadCommitted (\ _ -> put Condemned)

{-|
It goes without saying that SQL must not be transaction-related
like `BEGIN`, `COMMIT` or `ABORT`, otherwise you'll break the abstraction.
-}
sql :: Mode -> Level -> ByteString -> Transaction () ()
sql mode level sql = session mode level (\ _ -> Session.sql sql)

{-|
It goes without saying that the statement must not be transaction-related
like `BEGIN`, `COMMIT` or `ABORT`, otherwise you'll break the abstraction.
-}
statement :: Mode -> Level -> Statement i o -> Transaction i o
statement mode level statement = session mode level (\ input -> Session.statement input statement)

{-|
You must know that it is possible to break the abstraction,
if you execute statements such as `BEGIN` inside of the session,
or if you do IO there.
Therefore, you must have control over what's going on inside of the
executed session.
It's best for them to be defined as internal definitions inside of
your transactions, so that its stored in the same place and
is not possible to be affected by outside changes or be used elsewhere.
-}
session :: Mode -> Level -> (i -> Session o) -> Transaction i o
session mode level session = Transaction mode level (lift . session)
