module Hasql.Tx.Prelude
( 
  module Exports,
  bug,
  bottom,
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

-- contravariant
-------------------------
import Data.Functor.Contravariant as Exports
import Data.Functor.Contravariant.Divisible as Exports

-- either
-------------------------
import Control.Monad.Trans.Either as Exports
import Data.Either.Combinators as Exports

-- text
-------------------------
import Data.Text as Exports (Text)

-- bytestring
-------------------------
import Data.ByteString as Exports (ByteString)

-- placeholders
-------------------------
import Development.Placeholders as Exports

-- loch-th
-------------------------
import Debug.Trace.LocationTH as Exports

-- custom
-------------------------
import qualified Debug.Trace.LocationTH


bug =
  [e| $(Debug.Trace.LocationTH.failure) . (msg <>) |]
  where
    msg = "A \"hasql\" package bug: " :: String

bottom =
  [e| $bug "Bottom evaluated" |]
