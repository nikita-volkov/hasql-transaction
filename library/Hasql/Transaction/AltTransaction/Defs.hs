module Hasql.Transaction.AltTransaction.Defs where

import Hasql.Transaction.Prelude hiding (map)
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
data AltTransaction i o =
  AltTransaction Mode IsolationLevel [i -> StateT Condemnation Session o]

deriving instance Functor (AltTransaction i)

instance Applicative (AltTransaction i) where
  pure = AltTransaction Read ReadCommitted . pure . const . pure
  (<*>) = binOp $ \ lSession rSession i -> lSession i <*> rSession i

instance Alternative (AltTransaction i) where
  empty = AltTransaction Read ReadCommitted []
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
-}
condemn :: AltTransaction () ()
condemn = AltTransaction Read ReadCommitted [\ _ -> put Condemned]

{-|
It goes without saying that the statement must not be transaction-related
like `BEGIN`, `COMMIT` or `ABORT`, otherwise you'll break the abstraction.
-}
sql :: Mode -> IsolationLevel -> ByteString -> AltTransaction () ()
sql mode level sql = session mode level $ \ _ -> Session.sql sql

{-|
It goes without saying that the statement must not be transaction-related
like `BEGIN`, `COMMIT` or `ABORT`, otherwise you'll break the abstraction.
-}
statement :: Mode -> IsolationLevel -> Statement i o -> AltTransaction i o
statement mode level statement = session mode level $ \ i -> Session.statement i statement

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
session :: Mode -> IsolationLevel -> (i -> Session o) -> AltTransaction i o
session mode level sessionFn = AltTransaction mode level [lift . sessionFn]
