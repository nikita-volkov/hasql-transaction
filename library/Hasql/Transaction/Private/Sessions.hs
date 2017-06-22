module Hasql.Transaction.Private.Sessions
where

import Hasql.Transaction.Private.Prelude
import Hasql.Transaction.Private.Model
import qualified Hasql.Session as A
import qualified Hasql.Transaction.Private.Queries as B


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
        A.query () (B.beginTransaction isolation mode)
        (result, commit) <- session
        if commit
          then A.query () B.commitTransaction
          else A.query () B.abortTransaction
        return result
    onError continue error =
      do
        A.query () B.abortTransaction
        case error of
          A.ResultError (A.ServerError "40001" _ _ _) ->
            continue
          _ ->
            throwError error
