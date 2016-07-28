module Main.Transactions where

import Rebase.Prelude
import Hasql.Transaction
import qualified Main.Queries as A


createSchema :: Transaction ()
createSchema =
  do
    query () A.createAccountTable

dropSchema :: Transaction ()
dropSchema =
  do
    query () A.dropAccountTable

transfer :: Int64 -> Int64 -> Scientific -> Transaction ()
transfer id1 id2 amount =
  do
    query (id1, amount) A.modifyBalance
    query (id2, negate amount) A.modifyBalance
    return ()

transferTimes :: Int -> Int64 -> Int64 -> Scientific -> Transaction ()
transferTimes times id1 id2 amount =
  replicateM_ times (transfer id1 id2 amount)
