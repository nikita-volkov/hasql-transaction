module Hasql.Transaction.Private.Sessions where

import Hasql.Session
import Hasql.Transaction.Config
import Hasql.Transaction.Private.Prelude
import Hasql.Transaction.Private.Statements qualified as Statements

{-
We may want to
do one transaction retry in case of the 23505 error, and fail if an identical
error is seen.
-}
inRetryingTransaction :: IsolationLevel -> Mode -> Session (a, Bool) -> Session a
inRetryingTransaction level mode session =
  fix $ \retry -> do
    attemptRes <- tryTransaction level mode session
    case attemptRes of
      Just a -> return a
      Nothing -> retry

tryTransaction :: IsolationLevel -> Mode -> Session (a, Bool) -> Session (Maybe a)
tryTransaction level mode body = do
  statement () (Statements.beginTransaction level mode)

  bodyRes <- catchError (fmap Just body) $ \error -> do
    statement () Statements.abortTransaction
    handleTransactionError error $ return Nothing

  case bodyRes of
    Just (res, commit) -> catchError (commitOrAbort commit $> Just res) $ \error -> do
      handleTransactionError error $ return Nothing
    Nothing -> return Nothing

commitOrAbort :: Bool -> Session ()
commitOrAbort commit =
  if commit
    then statement () Statements.commitTransaction
    else statement () Statements.abortTransaction

handleTransactionError :: SessionError -> Session a -> Session a
handleTransactionError error onTransactionError = case error of
  QueryError _ _ clientError -> onCommandError clientError
  PipelineError clientError -> onCommandError clientError
  where
    onCommandError = \case
      ResultError (ServerError code _ _ _ _) ->
        case code of
          "40001" -> onTransactionError
          "40P01" -> onTransactionError
          _ -> throwError error
      _ -> throwError error
