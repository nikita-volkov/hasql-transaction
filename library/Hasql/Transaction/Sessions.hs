module Hasql.Transaction.Sessions
where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Model
import Hasql.Session
import qualified Hasql.Transaction.Statements as Statements


{-
We may want to
do one transaction retry in case of the 23505 error, and fail if an identical
error is seen.
-}
inRetryingTransaction :: IsolationLevel -> Mode -> Session (a, Bool) -> Session a
inRetryingTransaction isolation mode session =
  fix $ \recur -> catchError normal (onError recur)
  where
    normal =
      do
        statement () (Statements.beginTransaction isolation mode)
        (result, commit) <- session
        if commit
          then statement () Statements.commitTransaction
          else statement () Statements.abortTransaction
        return result
    onError continue error =
      do
        statement () Statements.abortTransaction
        case error of
          QueryError _ _ (ResultError (ServerError "40001" _ _ _)) ->
            continue
          _ ->
            throwError error
