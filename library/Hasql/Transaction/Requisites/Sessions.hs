{-|

TODO:
We may want to
do one transaction retry in case of the 23505 error, and fail if an identical
error is seen.
-}
module Hasql.Transaction.Requisites.Sessions
where

import Hasql.Transaction.Prelude
import Hasql.Transaction.Requisites.Model
import Hasql.Session
import qualified ListT
import qualified Hasql.Transaction.Requisites.Statements as Statements


inRetryingTransaction :: Mode -> IsolationLevel -> ListT (StateT Condemnation Session) a -> Session a
inRetryingTransaction mode level session =
  let
    loop session = do
      tryTransaction mode level (runStateT (ListT.uncons session) Uncondemned) >>= \ case
        Just unconsingResult -> case unconsingResult of
          Just (a, nextListT) -> undefined
          Nothing -> undefined
        Nothing -> error "TODO: Handle conflict"
      undefined
    in loop session

tryTransaction :: Mode -> IsolationLevel -> Session (a, Condemnation) -> Session (Maybe a)
tryTransaction mode level session = do
  statement () (Statements.beginTransaction mode level)
  catchError
    (do
      (result, condemnation) <- session
      case condemnation of
        Uncondemned -> statement () Statements.commitTransaction
        Condemned -> statement () Statements.abortTransaction
      return (Just result))
    (\ error -> do
      statement () Statements.abortTransaction
      case error of
        QueryError _ _ (ResultError (ServerError "40001" _ _ _)) -> return Nothing
        error -> throwError error)
