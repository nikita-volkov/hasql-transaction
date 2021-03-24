module Main where

import Prelude
import qualified Hasql.Connection as A
import qualified Hasql.Session as B
import qualified Hasql.Transaction as C
import qualified Hasql.Transaction.Sessions as G
import qualified Main.Statements as D
import qualified Main.Transactions as E
import qualified Control.Concurrent.Async as F


main =
  bracket acquire release use
  where
    acquire =
      (,) <$> acquire <*> acquire
      where
        acquire =
          join $
          fmap (either (fail . show) return) $
          A.acquire connectionSettings
          where
            connectionSettings =
              A.settings "localhost" 5432 "postgres" "" "postgres"
    release (connection1, connection2) =
      do
        transaction connection1 E.dropSchema
        A.release connection1
        A.release connection2
    use (connection1, connection2) =
      do
        try (transaction connection1 E.dropSchema) :: IO (Either SomeException ())
        transaction connection1 E.createSchema
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
  B.run session connection >>=
  either (fail . show) return

transaction connection transaction =
  session connection (G.transaction G.RepeatableRead G.Write transaction)


type Test =
  A.Connection -> A.Connection -> IO Bool

transactionsTest :: Test
transactionsTest connection1 connection2 =
  do
    id1 <- session connection1 (B.statement 0 D.createAccount)
    id2 <- session connection1 (B.statement 0 D.createAccount)
    async1 <- F.async (replicateM_ 1000 (transaction connection1 (E.transfer id1 id2 1)))
    async2 <- F.async (replicateM_ 1000 (transaction connection2 (E.transfer id1 id2 1)))
    F.wait async1
    F.wait async2
    balance1 <- session connection1 (B.statement id1 D.getBalance)
    balance2 <- session connection1 (B.statement id2 D.getBalance)
    traceShowM balance1
    traceShowM balance2
    return (balance1 == Just 2000 && balance2 == Just (-2000))

readAndWriteTransactionsTest :: Test
readAndWriteTransactionsTest connection1 connection2 =
  do
    id1 <- session connection1 (B.statement 0 D.createAccount)
    id2 <- session connection1 (B.statement 0 D.createAccount)
    async1 <- F.async (replicateM_ 1000 (transaction connection1 (E.transfer id1 id2 1)))
    async2 <- F.async (replicateM_ 1000 (transaction connection2 (C.statement id1 D.getBalance)))
    F.wait async1
    F.wait async2
    balance1 <- session connection1 (B.statement id1 D.getBalance)
    balance2 <- session connection1 (B.statement id2 D.getBalance)
    traceShowM balance1
    traceShowM balance2
    return (balance1 == Just 1000 && balance2 == Just (-1000))

transactionAndQueryTest :: Test
transactionAndQueryTest connection1 connection2 =
  do
    id1 <- session connection1 (B.statement 0 D.createAccount)
    id2 <- session connection1 (B.statement 0 D.createAccount)
    async1 <- F.async (transaction connection1 (E.transferTimes 200 id1 id2 1))
    async2 <- F.async (session connection2 (replicateM_ 200 (B.statement (id1, 1) D.modifyBalance)))
    F.wait async1
    F.wait async2
    balance1 <- session connection1 (B.statement id1 D.getBalance)
    balance2 <- session connection1 (B.statement id2 D.getBalance)
    traceShowM balance1
    traceShowM balance2
    return (balance1 == Just 400 && balance2 == Just (-200))
