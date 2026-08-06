module Helpers.Transactions where

import Hasql.Transaction
import Helpers.Statements qualified as Statements
import Prelude

createSchema :: Transaction ()
createSchema =
  statement () Statements.createAccountTable

dropSchema :: Transaction ()
dropSchema =
  statement () Statements.dropAccountTable

transfer :: Int64 -> Int64 -> Scientific -> Transaction Bool
transfer id1 id2 amount =
  do
    success <- statement (id1, amount) Statements.modifyBalance
    if success
      then statement (id2, negate amount) Statements.modifyBalance
      else return False

transferTimes :: Int -> Int64 -> Int64 -> Scientific -> Transaction ()
transferTimes times id1 id2 amount =
  replicateM_ times (transfer id1 id2 amount)
