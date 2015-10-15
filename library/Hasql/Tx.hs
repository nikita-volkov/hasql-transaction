module Hasql.Tx
where

import Hasql.Tx.Prelude
import qualified Hasql.Connection as Connection
import qualified Hasql.Query as Query
import qualified Hasql.ErrorCodes as ErrorCodes
import qualified Hasql.Tx.Queries as Queries


newtype Tx a =
  Tx (ReaderT (Connection.Connection, IORef Int) (EitherT Connection.ResultsError IO) a)
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

run :: Tx a -> Connection.Connection -> IsolationLevel -> Mode -> IO (Either Connection.ResultsError a)
run (Tx tx) connection isolation mode =
  runEitherT $ do
    EitherT $ Connection.executeParametricQuery connection (Queries.beginTransaction mode') ()
    counterRef <- lift $ newIORef 0
    resultEither <- lift $ runEitherT $ runReaderT tx (connection, counterRef)
    case resultEither of
      Left (Connection.ResultError (Connection.ServerError code _ _ _))
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
            EitherT $ Connection.executeParametricQuery connection query ()
        pure result
  where
    mode' =
      (unsafeCoerce isolation, write)
    (write, commit) =
      case mode of
        Read -> (False, True)
        Write -> (True, True)
        WriteWithoutCommitting -> (True, False)

parametricQuery :: Query.ParametricQuery a b -> a -> Tx b
parametricQuery query params =
  Tx $ ReaderT $ \(connection, _) -> EitherT $
  Connection.executeParametricQuery connection query params
