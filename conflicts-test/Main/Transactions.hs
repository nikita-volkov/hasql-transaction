module Main.Transactions where

import Prelude
import Hasql.Transaction
import qualified Main.Statements as A


createSchema :: Transaction ()
createSchema = statement Write Serializable A.createAccountTable ()

dropSchema :: Transaction ()
dropSchema = statement Write Serializable A.dropAccountTable ()

transfer :: Int64 -> Int64 -> Scientific -> Transaction Bool
transfer id1 id2 amount = do
  ifS
    (statement Write Serializable A.modifyBalance (id1, amount))
    (statement Write Serializable A.modifyBalance (id2, negate amount))
    (pure False)

transferTimes :: Int -> Int64 -> Int64 -> Scientific -> Transaction ()
transferTimes times id1 id2 amount = replicateM_ times (transfer id1 id2 amount)

getBalance :: Int64 -> Transaction (Maybe Scientific)
getBalance id = statement Read ReadCommitted A.getBalance id
