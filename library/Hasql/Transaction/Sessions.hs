{-|

TODO:
We may want to
do one transaction retry in case of the 23505 error, and fail if an identical
error is seen.
-}
module Hasql.Transaction.Sessions
where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Types
import Hasql.Session
import qualified Hasql.Transaction.Statements as Statements


inAlternatingTransaction :: Mode -> Level -> [Session (a, Condemnation)] -> Session a
inAlternatingTransaction mode level sessions =
  let
    loop = \ case
      session : sessionsTail -> tryTransaction mode level session >>= \ case
        Just a -> return a
        Nothing -> loop sessionsTail
      _ -> loop sessions
    in loop sessions

tryTransaction :: Mode -> Level -> Session (a, Condemnation) -> Session (Maybe a)
tryTransaction mode level body = do

  statement () (Statements.beginTransaction mode level)

  bodyRes <- catchError (fmap Just body) $ \ error -> do
    statement () Statements.abortTransaction
    handleTransactionError error $ return Nothing

  case bodyRes of
    Just (res, commit) -> catchError (commitOrAbort commit $> Just res) $ \ error -> do
      handleTransactionError error $ return Nothing
    Nothing -> return Nothing

commitOrAbort = \ case
  Uncondemned -> statement () Statements.commitTransaction
  Condemned -> statement () Statements.abortTransaction

handleTransactionError error onTransactionError = case error of
  QueryError _ _ (ResultError (ServerError "40001" _ _ _)) -> onTransactionError
  error -> throwError error