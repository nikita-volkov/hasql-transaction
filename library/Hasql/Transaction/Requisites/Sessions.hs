module Hasql.Transaction.Requisites.Sessions
where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Requisites.Model
import Hasql.Session
import qualified Hasql.Transaction.Requisites.Statements as Statements


{-
We may want to
do one transaction retry in case of the 23505 error, and fail if an identical
error is seen.
-}
inRetryingTransaction :: IsolationLevel -> Mode -> Session (a, Condemnation) -> Session a
inRetryingTransaction isolation mode session =
  fix $ \recur -> catchError normal (onError recur)
  where
    normal =
      do
        statement () (Statements.beginTransaction isolation mode)
        (result, condemnation) <- session
        if condemnation == Uncondemned
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
