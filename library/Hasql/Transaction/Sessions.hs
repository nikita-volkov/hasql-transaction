module Hasql.Transaction.Sessions
where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Model
import qualified Hasql.Session as A
import qualified Hasql.Transaction.Statements as B


{-
We may want to
do one transaction retry in case of the 23505 error, and fail if an identical
error is seen.
-}
inRetryingTransaction :: IsolationLevel -> Mode -> A.Session (a, Bool) -> A.Session a
inRetryingTransaction isolation mode session =
  fix $ \recur -> catchError normal (onError recur)
  where
    normal =
      do
        A.statement () (B.beginTransaction isolation mode)
        (result, commit) <- session
        if commit
          then A.statement () B.commitTransaction
          else A.statement () B.abortTransaction
        return result
    onError continue error =
      do
        A.statement () B.abortTransaction
        case error of
          A.QueryError _ _ (A.ResultError (A.ServerError "40001" _ _ _)) ->
            continue
          _ ->
            throwError error
