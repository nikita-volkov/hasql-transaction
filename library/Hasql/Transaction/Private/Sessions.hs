module Hasql.Transaction.Private.Sessions
where

import Hasql.Transaction.Private.Prelude
import Hasql.Transaction.Private.Model
import Hasql.Session
import qualified Hasql.Transaction.Private.Statements as Statements


{-
We may want to
do one transaction retry in case of the 23505 error, and fail if an identical
error is seen.
-}
inRetryingTransaction :: IsolationLevel -> Mode -> Session (a, Bool) -> Session a
inRetryingTransaction level mode session =
  fix $ \ retry -> do
    attemptRes <- tryTransaction level mode session
    case attemptRes of
      Just a -> return a
      Nothing -> retry

tryTransaction :: IsolationLevel -> Mode -> Session (a, Bool) -> Session (Maybe a)
tryTransaction level mode body = do

  statement () (Statements.beginTransaction level mode)

  bodyRes <- catchError (fmap Just body) $ \ error -> do
    statement () Statements.abortTransaction
    handleTransactionError error $ return Nothing

  case bodyRes of
    Just (res, commit) -> catchError (commitOrAbort commit $> Just res) $ \ error -> do
      handleTransactionError error $ return Nothing
    Nothing -> return Nothing

commitOrAbort commit = if commit
  then statement () Statements.commitTransaction
  else statement () Statements.abortTransaction

handleTransactionError error onTransactionError = case error of
  QueryError _ _ (ResultError (ServerError "40001" _ _ _)) -> onTransactionError
  error -> throwError error
