module Main.Transactions where

import Rebase.Prelude
import Hasql.Transaction
import qualified Main.Queries as Queries


createSchema :: Transaction ()
createSchema =
  do
    query () Queries.createAccountTable

dropSchema :: Transaction ()
dropSchema =
  do
    query () Queries.dropAccountTable

transfer :: Int64 -> Int64 -> Scientific -> Transaction ()
transfer id1 id2 amount =
  do
    query (id1, amount) Queries.modifyBalance
    query (id2, negate amount) Queries.modifyBalance
    return ()

transferTimes :: Int -> Int64 -> Int64 -> Scientific -> Transaction ()
transferTimes times id1 id2 amount =
  replicateM_ times (transfer id1 id2 amount)
