module Specs.ConflictsSpec where

import Control.Concurrent.Async qualified as Async
import Hasql.Session qualified as Session
import Hasql.Transaction qualified as Transaction
import Helpers.Scripts qualified as Scripts
import Helpers.Statements qualified as Statements
import Helpers.Transactions qualified as Transactions
import Prelude
import Test.Hspec

spec :: SpecWith Scripts.ScopeParams
spec = do
  it "Retrying transactions converge to a consistent balance under contention" \scopeParams ->
    Scripts.onConnectionPair scopeParams \connection1 connection2 -> do
      id1 <- Scripts.session connection1 (Session.statement 0 Statements.createAccount)
      id2 <- Scripts.session connection1 (Session.statement 0 Statements.createAccount)
      async1 <- Async.async (replicateM_ 1000 (Scripts.transaction connection1 (Transactions.transfer id1 id2 1)))
      async2 <- Async.async (replicateM_ 1000 (Scripts.transaction connection2 (Transactions.transfer id1 id2 1)))
      Async.wait async1
      Async.wait async2
      balance1 <- Scripts.session connection1 (Session.statement id1 Statements.getBalance)
      balance2 <- Scripts.session connection1 (Session.statement id2 Statements.getBalance)
      balance1 `shouldBe` Just 2000
      balance2 `shouldBe` Just (-2000)

  it "Non-retrying transactions fail with a serialization error under contention" \scopeParams ->
    Scripts.onConnectionPair scopeParams \connection1 connection2 -> do
      id1 <- Scripts.session connection1 (Session.statement 0 Statements.createAccount)
      id2 <- Scripts.session connection1 (Session.statement 0 Statements.createAccount)
      async1 <- Async.async (replicateM_ 1000 (Scripts.transactionNoRetry connection1 (Transactions.transfer id1 id2 1)))
      async2 <- Async.async (replicateM_ 1000 (Scripts.transactionNoRetry connection2 (Transactions.transfer id1 id2 1)))
      result1 <- Async.waitCatch async1
      result2 <- Async.waitCatch async2
      let serialError = sequenceA [result1, result2]
      serialError `shouldSatisfy` either (isInfixOf "40001" . show) (const False)

  it "A concurrent read-only transaction does not lose updates from a writer" \scopeParams ->
    Scripts.onConnectionPair scopeParams \connection1 connection2 -> do
      id1 <- Scripts.session connection1 (Session.statement 0 Statements.createAccount)
      id2 <- Scripts.session connection1 (Session.statement 0 Statements.createAccount)
      async1 <- Async.async (replicateM_ 1000 (Scripts.transaction connection1 (Transactions.transfer id1 id2 1)))
      async2 <- Async.async (replicateM_ 1000 (Scripts.transaction connection2 (Transaction.statement id1 Statements.getBalance)))
      Async.wait async1
      Async.wait async2
      balance1 <- Scripts.session connection1 (Session.statement id1 Statements.getBalance)
      balance2 <- Scripts.session connection1 (Session.statement id2 Statements.getBalance)
      balance1 `shouldBe` Just 1000
      balance2 `shouldBe` Just (-1000)

  it "A transaction converges to a consistent balance alongside a concurrent bare session" \scopeParams ->
    Scripts.onConnectionPair scopeParams \connection1 connection2 -> do
      id1 <- Scripts.session connection1 (Session.statement 0 Statements.createAccount)
      id2 <- Scripts.session connection1 (Session.statement 0 Statements.createAccount)
      async1 <- Async.async (Scripts.transaction connection1 (Transactions.transferTimes 200 id1 id2 1))
      async2 <- Async.async (Scripts.session connection2 (replicateM_ 200 (Session.statement (id1, 1) Statements.modifyBalance)))
      Async.wait async1
      Async.wait async2
      balance1 <- Scripts.session connection1 (Session.statement id1 Statements.getBalance)
      balance2 <- Scripts.session connection1 (Session.statement id2 Statements.getBalance)
      balance1 `shouldBe` Just 400
      balance2 `shouldBe` Just (-200)
