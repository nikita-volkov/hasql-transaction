module Hasql.Transaction.Prelude
( 
  module Exports,
  tryError,
)
where


-- base-prelude
-------------------------
import BasePrelude as Exports hiding (assert, left, right, isLeft, isRight, error)

-- transformers
-------------------------
import Control.Monad.IO.Class as Exports
import Control.Monad.Trans.Class as Exports
import Control.Monad.Trans.Maybe as Exports hiding (liftListen, liftPass)
import Control.Monad.Trans.Reader as Exports hiding (liftCallCC, liftCatch)
import Control.Monad.Trans.State.Strict as Exports hiding (liftCallCC, liftCatch, liftListen, liftPass)

-- mtl
-------------------------
import Control.Monad.Error.Class as Exports (MonadError (..))

-- contravariant
-------------------------
import Data.Functor.Contravariant as Exports
import Data.Functor.Contravariant.Divisible as Exports

-- contravariant-extras
-------------------------
import Contravariant.Extras as Exports

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

tryError :: MonadError e m => m a -> m (Either e a)
tryError m =
  catchError (liftM Right m) (return . Left)
