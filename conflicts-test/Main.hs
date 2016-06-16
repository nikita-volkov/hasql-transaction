module Main where

import Rebase.Prelude
import qualified Hasql.Connection as Connection
import qualified Hasql.Session as Session
import qualified Hasql.Transaction as Transaction
import qualified Main.Queries as Queries
import qualified Main.Transactions as Transactions
import qualified Control.Concurrent.Async as Async


main =
  bracket acquire release use
  where
    acquire =
      (,) <$> acquire <*> acquire
      where
        acquire =
          join $
          fmap (either (fail . show) return) $
          Connection.acquire connectionSettings
          where
            connectionSettings =
              Connection.settings "localhost" 5432 "postgres" "" "postgres"
    release (connection1, connection2) =
      do
        transaction connection1 Transactions.dropSchema
        Connection.release connection1
        Connection.release connection2
    use (connection1, connection2) =
      do
        try (transaction connection1 Transactions.dropSchema) :: IO (Either SomeException ())
        transaction connection1 Transactions.createSchema
        id1 <- session connection1 (Session.query 0 Queries.createAccount)
        id2 <- session connection1 (Session.query 0 Queries.createAccount)
        async1 <- Async.async (transaction connection1 (Transactions.transferTimes 200 id1 id2 1))
        async2 <- Async.async (transaction connection2 (Transactions.transferTimes 200 id1 id2 1))
        Async.wait async1
        Async.wait async2
        balance1 <- session connection1 (Session.query id1 Queries.getBalance)
        balance2 <- session connection1 (Session.query id2 Queries.getBalance)
        traceShowM balance1
        traceShowM balance2
        if balance1 == Just 400 && balance2 == Just (-400)
          then exitSuccess
          else exitFailure

session connection session =
  Session.run session connection >>=
  either (fail . show) return

transaction connection transaction =
  session connection (Transaction.run transaction Transaction.RepeatableRead Transaction.Write)
