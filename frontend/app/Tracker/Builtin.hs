module Tracker.Builtin (
    TrackerSelection (..),
    Ident (..),
    getTracker
) where

import Tracker
import Tracker.Display
import qualified Tracker.TMCR.Tracker as TMCR
import Control.Monad.Choice

data TrackerSelection = Builtin Ident deriving (Ord, Eq, Show)

data Ident = TMCR deriving (Ord, Eq, Enum, Show)

getTracker :: (MonadChoice m) => TrackerSelection -> m (Tracker, TrackerDisplayData)
getTracker (Builtin TMCR) = do
    tracker <- TMCR.getTracker
    disp <- TMCR.getTrackerDisplayData
    return (tracker, disp)

