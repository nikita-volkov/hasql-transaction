module Hasql.Tx
where

import Hasql.Tx.Prelude
import qualified Hasql
import qualified Hasql.Tx.Queries as Queries
import qualified PostgreSQL.ErrorCodes as ErrorCodes


newtype Tx a =
  Tx (ReaderT (Hasql.Connection, IORef Int) (EitherT Hasql.ResultsError IO) a)
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
  -- and to let Hasql roll all the changes back on the exit from the transaction.
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

run :: Tx a -> Hasql.Connection -> IsolationLevel -> Mode -> IO (Either Hasql.ResultsError a)
run (Tx tx) connection isolation mode =
  runEitherT $ do
    EitherT $ Hasql.query connection (Queries.beginTransaction mode') ()
    counterRef <- lift $ newIORef 0
    resultEither <- lift $ runEitherT $ runReaderT tx (connection, counterRef)
    case resultEither of
      Left (Hasql.ResultError (Hasql.ServerError code _ _ _))
        | code == ErrorCodes.serialization_failure ->
          EitherT $ run (Tx tx) connection isolation mode
      _ -> do
        result <- EitherT $ pure resultEither
        let
          query =
            if commit
              then Queries.commitTransaction
              else Queries.abortTransaction
          in
            EitherT $ Hasql.query connection query ()
        pure result
  where
    mode' =
      (unsafeCoerce isolation, write)
    (write, commit) =
      case mode of
        Read -> (False, True)
        Write -> (True, True)
        WriteWithoutCommitting -> (True, False)

query :: Hasql.Query a b -> a -> Tx b
query query params =
  Tx $ ReaderT $ \(connection, _) -> EitherT $
  Hasql.query connection query params
