module Helpers.Scripts
  ( ScopeParams,
    onConnectionPair,
    session,
    transaction,
    transactionNoRetry,
  )
where

import Hasql.Connection qualified as Connection
import Hasql.Connection.Settings qualified as Settings
import Hasql.Session qualified as Session
import Hasql.Transaction qualified as Transaction
import Hasql.Transaction.Sessions qualified as Transaction.Sessions
import Helpers.Transactions qualified as Transactions
import Pqi qualified
import Prelude

-- |
-- Adapter, host and port of a running isolated postgres server.
type ScopeParams = (Pqi.Adapter, Text, Word16)

-- |
-- Acquire a pair of connections against a fresh copy of the schema,
-- releasing them and dropping the schema once the action completes.
onConnectionPair :: ScopeParams -> (Connection.Connection -> Connection.Connection -> IO ()) -> IO ()
onConnectionPair (adapter, host, port) action =
  bracket acquire release use
  where
    acquire =
      (,) <$> acquireConnection <*> acquireConnection
      where
        acquireConnection =
          Connection.acquire adapter connectionSettings
            >>= either (fail . show) return
        connectionSettings =
          Settings.hostAndPort host port
            <> Settings.user "postgres"
            <> Settings.password "postgres"
            <> Settings.dbname "postgres"
    release (connection1, connection2) = do
      transaction connection1 Transactions.dropSchema
      Connection.release connection1
      Connection.release connection2
    use (connection1, connection2) = do
      _ <- try (transaction connection1 Transactions.dropSchema) :: IO (Either SomeException ())
      transaction connection1 Transactions.createSchema
      action connection1 connection2

session :: Connection.Connection -> Session.Session a -> IO a
session connection theSession =
  Connection.use connection theSession
    >>= either (fail . show) return

transaction :: Connection.Connection -> Transaction.Transaction a -> IO a
transaction connection theTransaction =
  session connection (Transaction.Sessions.transaction Transaction.Sessions.RepeatableRead Transaction.Sessions.Write theTransaction)

transactionNoRetry :: Connection.Connection -> Transaction.Transaction a -> IO a
transactionNoRetry connection theTransaction =
  session connection (Transaction.Sessions.transactionNoRetry Transaction.Sessions.RepeatableRead Transaction.Sessions.Write theTransaction)
