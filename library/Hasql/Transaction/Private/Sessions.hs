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
inRetryingTransaction :: IsolationLevel -> Mode -> Session (a, Bool) -> Bool -> Session a
inRetryingTransaction level mode session preparable =
  fix $ \ retry -> do
    attemptRes <- tryTransaction level mode session preparable
    case attemptRes of
      Just a -> return a
      Nothing -> retry

tryTransaction :: IsolationLevel -> Mode -> Session (a, Bool) -> Bool -> Session (Maybe a)
tryTransaction level mode body preparable = do

  statement () (Statements.beginTransaction level mode preparable)

  bodyRes <- catchError (fmap Just body) $ \ error -> do
    statement () (Statements.abortTransaction preparable)
    handleTransactionError error $ return Nothing

  case bodyRes of
    Just (res, commit) -> catchError (commitOrAbort commit preparable $> Just res) $ \ error -> do
      handleTransactionError error $ return Nothing
    Nothing -> return Nothing

commitOrAbort commit preparable = if commit
  then statement () (Statements.commitTransaction preparable)
  else statement () (Statements.abortTransaction preparable)

handleTransactionError error onTransactionError = case error of
  QueryError _ _ (ResultError (ServerError "40001" _ _ _)) -> onTransactionError
  error -> throwError error
