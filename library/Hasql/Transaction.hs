module Hasql.Transaction
(
  -- * Transaction settings
  Mode(..),
  IsolationLevel(..),
  -- * Transaction monad
  Transaction,
  run,
  query,
)
where

import Hasql.Transaction.Prelude
import qualified Hasql.Connection as Connection
import qualified Hasql.Query as Query
import qualified Hasql.Transaction.Queries as Queries
import qualified PostgreSQL.ErrorCodes as ErrorCodes


-- |
-- A composable abstraction over the retryable transactions.
-- 
-- Executes multiple queries under the specified mode and isolation level,
-- while automatically retrying the transaction in case of conflicts.
-- Thus this abstraction closely reproduces the behaviour of 'STM'.
newtype Transaction a =
  Transaction (ReaderT (Connection.Connection, IORef Int) (EitherT Query.ResultsError IO) a)
  deriving (Functor, Applicative, Monad)

-- |
-- 
data Mode =
  -- |
  -- Read-only. No writes possible.
  Read |
  -- |
  -- Write and commit.
  Write |
  -- |
  -- Write without committing.
  -- Useful for testing, 
  -- allowing you to modify your database, 
  -- producing some result based on your changes,
  -- and letting Hasql roll all the changes back on the exit from the transaction.
  WriteWithoutCommitting
  deriving (Show, Eq, Ord, Enum, Bounded)

-- |
-- For reference see
-- <http://www.postgresql.org/docs/current/static/transaction-iso.html the Postgres' documentation>.
-- 
data IsolationLevel =
  ReadCommitted |
  RepeatableRead |
  Serializable
  deriving (Show, Eq, Ord, Enum, Bounded)

-- |
-- Execute the transaction using the provided isolation level, mode and database connection.
{-# INLINABLE run #-}
run :: Transaction a -> IsolationLevel -> Mode -> Connection.Connection -> IO (Either Query.ResultsError a)
run (Transaction tx) isolation mode connection =
  runEitherT $ do
    EitherT $ Query.run (Queries.beginTransaction mode') () connection
    counterRef <- lift $ newIORef 0
    resultEither <- lift $ runEitherT $ runReaderT tx (connection, counterRef)
    case resultEither of
      Left (Query.ResultError (Query.ServerError code _ _ _))
        | code == ErrorCodes.serialization_failure ->
          EitherT $ run (Transaction tx) isolation mode connection
      _ -> do
        result <- EitherT $ pure resultEither
        let
          query =
            if commit
              then Queries.commitTransaction
              else Queries.abortTransaction
          in
            EitherT $ Query.run query () connection
        pure result
  where
    mode' =
      (unsafeCoerce isolation, write)
    (write, commit) =
      case mode of
        Read -> (False, True)
        Write -> (True, True)
        WriteWithoutCommitting -> (True, False)

-- |
-- Execute a query in the context of a transaction.
{-# INLINABLE query #-}
query :: a -> Query.Query a b -> Transaction b
query params query =
  Transaction $ ReaderT $ \(connection, _) -> EitherT $
  Query.run query params connection
