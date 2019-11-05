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
