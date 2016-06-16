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
        success <- fmap and (traverse runTest tests)
        if success
          then exitSuccess
          else exitFailure
      where
        runTest test =
          test connection1 connection2
        tests =
          [readAndWriteTransactionsTest, transactionsTest, transactionAndQueryTest]


session connection session =
  Session.run session connection >>=
  either (fail . show) return

transaction connection transaction =
  session connection (Transaction.run transaction Transaction.RepeatableRead Transaction.Write)


type Test =
  Connection.Connection -> Connection.Connection -> IO Bool

transactionsTest :: Test
transactionsTest connection1 connection2 =
  do
    id1 <- session connection1 (Session.query 0 Queries.createAccount)
    id2 <- session connection1 (Session.query 0 Queries.createAccount)
    async1 <- Async.async (replicateM_ 1000 (transaction connection1 (Transactions.transfer id1 id2 1)))
    async2 <- Async.async (replicateM_ 1000 (transaction connection2 (Transactions.transfer id1 id2 1)))
    Async.wait async1
    Async.wait async2
    balance1 <- session connection1 (Session.query id1 Queries.getBalance)
    balance2 <- session connection1 (Session.query id2 Queries.getBalance)
    traceShowM balance1
    traceShowM balance2
    return (balance1 == Just 2000 && balance2 == Just (-2000))

readAndWriteTransactionsTest :: Test
readAndWriteTransactionsTest connection1 connection2 =
  do
    id1 <- session connection1 (Session.query 0 Queries.createAccount)
    id2 <- session connection1 (Session.query 0 Queries.createAccount)
    async1 <- Async.async (replicateM_ 1000 (transaction connection1 (Transactions.transfer id1 id2 1)))
    async2 <- Async.async (replicateM_ 1000 (transaction connection2 (Transaction.query id1 Queries.getBalance)))
    Async.wait async1
    Async.wait async2
    balance1 <- session connection1 (Session.query id1 Queries.getBalance)
    balance2 <- session connection1 (Session.query id2 Queries.getBalance)
    traceShowM balance1
    traceShowM balance2
    return (balance1 == Just 1000 && balance2 == Just (-1000))

transactionAndQueryTest :: Test
transactionAndQueryTest connection1 connection2 =
  do
    id1 <- session connection1 (Session.query 0 Queries.createAccount)
    id2 <- session connection1 (Session.query 0 Queries.createAccount)
    async1 <- Async.async (transaction connection1 (Transactions.transferTimes 200 id1 id2 1))
    async2 <- Async.async (session connection2 (replicateM_ 200 (Session.query (id1, 1) Queries.modifyBalance)))
    Async.wait async1
    Async.wait async2
    balance1 <- session connection1 (Session.query id1 Queries.getBalance)
    balance2 <- session connection1 (Session.query id2 Queries.getBalance)
    traceShowM balance1
    traceShowM balance2
    return (balance1 == Just 400 && balance2 == Just (-200))
