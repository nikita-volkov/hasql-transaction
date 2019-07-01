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
import qualified Hasql.Transaction.Requisites.Statements as Statements


inRetryingTransaction :: Mode -> Level -> Session (a, Condemnation) -> Session a
inRetryingTransaction mode isolation session =
  fix $ \ recur -> catchError normal (onError recur)
  where
    normal =
      do
        statement () (Statements.beginTransaction mode isolation)
        (result, condemnation) <- session
        case condemnation of
          Uncondemned -> statement () Statements.commitTransaction
          Condemned -> statement () Statements.abortTransaction
        return result
    onError continue error =
      do
        statement () Statements.abortTransaction
        case error of
          QueryError _ _ (ResultError (ServerError "40001" _ _ _)) -> continue
          _ -> throwError error

inAlternatingTransaction :: Mode -> Level -> [Session (a, Condemnation)] -> Session (Maybe a)
inAlternatingTransaction mode level sessions =
  let
    loop = \ case
      session : sessionsTail -> tryTransaction mode level session >>= \ case
        Just a -> return (Just a)
        Nothing -> loop sessionsTail
      _ -> case sessions of
        _ : _ -> loop sessions
        _ -> return Nothing
    in loop sessions

tryTransaction :: Mode -> Level -> Session (a, Condemnation) -> Session (Maybe a)
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
