{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
module Tracker where

import Tracker.Logic
import qualified Data.Map as Map
import Data.Map (Map())
import qualified Data.Set as Set
import Data.Set (Set())
import Control.Arrow
import Data.Bool (bool)
import Control.Lens
import Data.Text (Text(), pack)
import Control.Monad
import Control.Applicative
import Reflex
import Reflex.Dom ((=:))
import Data.Maybe
import Data.Monoid (Dual(..))

import Data.Trie
import Reflex.Dom.Image

import Control.Monad.Choice
import Data.FileEmbed
import Data.Default.Class

import Data.Reflection

_Singleton :: (Ord a) => Simple Prism (Set a) a
_Singleton = prism' (Set.singleton) (\s -> case Set.toList s of [a] -> Just a; _ -> Nothing)

buildTracker :: (Ord i, Ord l, Applicative f) => Map i (f Int) -> [Rule (Either i l) LogicExpr l] -> Map l (f Bool)
buildTracker items rules = locations where
    locations = fmap eval' $ Map.fromListWith (\a b -> Disjunction [a,b]) (fmap (_ruleHead &&& _ruleBody) rules)
    eval' = fmap eval . traverse (\case
                Left item -> Map.findWithDefault (pure 0) item items
                Right location -> fmap (bool 0 1) $ Map.findWithDefault (pure False) location locations)

type Scope = [Text]
type ItemName = Text
type LocationName = Text
type ItemState = Int
data LocationState = LocationState {
        _locationStateVisited :: Bool,
        _locationStateItem :: Maybe ItemName
        } deriving (Eq, Ord, Show, Read)
instance Default LocationState where
    def = LocationState False Nothing

data Tracker = Tracker {
    _trackerInterface :: TrackerInterface,
    _trackerExtras :: TrackerExtras,
    _trackerLogic :: [Rule (Either ItemName LocationName) LogicExpr LocationName]
    }

data TrackerInterface = TrackerInterface {
    _alwaysWidget :: TrackerWidgets,
    _subTrackers :: Trie Text TrackerWidgets
    }

data TrackerExtras = TrackerExtras {
    _untrackedItems :: [(ItemName, ItemState)],
    _nonLogicTrackedItems :: [ItemName],
    _itemMaxAmounts :: Trie Text (Map ItemName Int),
    _extrasComboItems :: Map ItemName ComboData,
    _itemAliases :: [(ItemName, ItemName)]
    }

instance Semigroup TrackerExtras where
    TrackerExtras a b c d e <> TrackerExtras a' b' c' d' e' = TrackerExtras (a <> a') (b <> b') (c <> c') (d <> d') (e <> e')

instance Monoid TrackerExtras where
    mempty = TrackerExtras mempty mempty mempty mempty mempty

data ComboData = ComboData {
        _comboItems :: [ItemName],
        _comboType :: ComboType
        }

data ComboType = SimpleCombo | ProductCombo

type TrackerWidgets = [TrackerWidget]

data TrackerWidget = TrackerWidgetGrid [[TrackerWidgetPrimitive]]
                   | TrackerWidgetMap LocationMap
                   | TrackerWidgetLocationList [Either Scope LocationName]

data TrackerWidgetPrimitive = SimpleItem ItemName
                            | SpecialLocation LocationName [ItemName]
                            | ComboItem ItemName
                            | Scope [Text]

data LocationMap = LocationMapNested {
    _locationMapSmall :: Image, --thumbnail/low screen resolution alternative
    _locationMapWith :: Int,
    _locationMapHeight :: Int,
    _locationMapNestedAreas :: Map Text (Int, Int, Int, Int, LocationMapArea) --posX,posY,width,height on the thumbnail, area (with offset/size for high-res combine)
    } | LocationMap LocationMapArea

data LocationMapArea = LocationMapArea {
    _locationMapAreaImage :: Image,
    _locationMapAreaOffsetX :: Int,
    _locationMapAreaOffsetY :: Int,
    _locationMapAreaWidth :: Int,
    _locationMapAreaHeight :: Int,
    _locationMapPins :: (Map (Int, Int) LocationMapPin)
    } deriving (Eq, Ord)

data LocationMapPin = LocationMapPinLocations [Either Scope LocationName]
                    | LocationMapPinDisplayItem (Either ItemName LocationName) [Either Scope LocationName]
                    deriving (Eq, Ord)

locationMapPinContents :: Simple Lens LocationMapPin [Either Scope LocationName]
locationMapPinContents = lens get set where
    get (LocationMapPinLocations l) = l
    get (LocationMapPinDisplayItem _ l) = l
    set (LocationMapPinLocations _) = LocationMapPinLocations
    set (LocationMapPinDisplayItem i _) = LocationMapPinDisplayItem i

locationMapPinDisplayItem :: Simple Lens LocationMapPin (Maybe (Either ItemName LocationName))
locationMapPinDisplayItem = lens get set where
    get (LocationMapPinLocations _) = Nothing
    get (LocationMapPinDisplayItem i _) = Just i
    set (LocationMapPinLocations l) Nothing = LocationMapPinLocations l
    set (LocationMapPinLocations l) (Just i) = LocationMapPinDisplayItem i l
    set (LocationMapPinDisplayItem _ l) Nothing = LocationMapPinLocations l
    set (LocationMapPinDisplayItem _ l) (Just i) = LocationMapPinDisplayItem i l

$(makeLenses ''Tracker)
$(makeLenses ''TrackerExtras)
$(makeLenses ''ComboData)
$(makeLenses ''TrackerInterface)
$(makeLenses ''LocationMap)
$(makeLenses ''LocationMapArea)
$(makePrisms ''TrackerWidgetPrimitive)

locationMapAreas :: Simple Traversal LocationMap LocationMapArea
locationMapAreas f (LocationMap area) = LocationMap <$> f area
locationMapAreas f (LocationMapNested i w h areas) = LocationMapNested i w h <$> (traverse . _5) f areas

locationMapAreas' :: LocationMap -> Set LocationMapArea
locationMapAreas' (LocationMap area) = Set.singleton area
locationMapAreas' (LocationMapNested _ _ _ areas) = areas ^. folded . _5 . to Set.singleton

getItemMaxCount :: ItemName -> TrackerExtras -> Int
getItemMaxCount i extras = fromMaybe 1 $ maximumOf (itemMaxAmounts . folded . ifolded . indices (match i)) extras where
                match i i' = i == i' || (i', i) `elem` extras ^. itemAliases

handledItems :: TrackerWidget -> Set ItemName
handledItems (TrackerWidgetMap _) = mempty
handledItems (TrackerWidgetLocationList _) = mempty
handledItems (TrackerWidgetGrid g) = Set.fromList $ join g >>= \case
        SimpleItem i -> return i
        SpecialLocation _ _ -> mempty
        ComboItem is -> return is
        Scope _ -> mempty

handledLocations :: TrackerWidget -> Set LocationName
handledLocations (TrackerWidgetMap m) = m ^. locationMapAreas . locationMapPins . folded . to pinLocations . folded . to Set.singleton
handledLocations (TrackerWidgetGrid g) = g ^. folded . folded . _SpecialLocation . _1 . to Set.singleton
handledLocations (TrackerWidgetLocationList locs) = locs ^. folded . _Right . to Set.singleton

pinLocations :: LocationMapPin -> [LocationName]
pinLocations (LocationMapPinLocations l) = l ^.. folded . _Right
pinLocations (LocationMapPinDisplayItem i l) = (i ^.. _Right) <> (l ^.. folded . _Right)

restrictToRelevantLocations :: Tracker -> Tracker
restrictToRelevantLocations tracker = tracker & trackerInterface %~ restrictTo relevantLocations where
        relevantLocations = tracker ^. trackerLogic . traverse . ruleHead . re (_Singleton)
        restrictTo locs intf = intf & alwaysWidget . traverse %~ restrictTo' locs & subTrackers . traverse . traverse %~ restrictTo' locs
        restrictTo' locs g@(TrackerWidgetGrid _) = g
        restrictTo' locs (TrackerWidgetLocationList l) = TrackerWidgetLocationList $ filter (either (const True) (`Set.member` locs)) l
        restrictTo' locs (TrackerWidgetMap m) = TrackerWidgetMap $ m & locationMapAreas . locationMapPins . traverse . locationMapPinContents %~ filter (either (const True) (`Set.member` locs)) & locationMapAreas . locationMapPins %~ (Map.filter (/= LocationMapPinLocations []))




data TrackerStateUpdateLog = LogIncrementItem ItemName
                           | LogDecrementItem ItemName
                           | LogSetItemCount ItemName Int Int
                           | LogMarkLocation LocationName
                           | LogUnmarkLocation LocationName
                           | LogSetItemAtLocation (Maybe ItemName) LocationName (Maybe ItemName)
                           | LogAutoAssignItemToLocation ItemName LocationName
                           | LogGroup (Maybe TrackerStateUpdateLog) [TrackerStateUpdateLog]
                           | LogSummary Text
                           deriving (Eq, Ord, Show)

--types subject to change, lenses should be stable interface
data TrackerState = TrackerState {
    _stateItems :: Map ItemName ItemState,
    _stateLocations :: Map LocationName LocationState,
    _stateCurrentScope :: Scope,
    _stateSelectedLocations :: Set (Either Scope LocationName),
    _stateLog :: [TrackerStateUpdateLog],
    _stateRedo :: Maybe TrackerState
    } deriving (Eq, Ord, Show)
$(makeLenses ''TrackerState)
itemStateAmount :: Simple Lens ItemState Int
itemStateAmount = id
$(makeLenses ''LocationState)

instance Default TrackerState where
    def = TrackerState mempty mempty mempty mempty mempty def

data TrackerStateUpdates = TrackerStateUpdates { applyTrackerStateUpdates :: TrackerExtras -> TrackerState -> Maybe TrackerState }

instance Semigroup TrackerStateUpdates where
    TrackerStateUpdates u <> TrackerStateUpdates u' = TrackerStateUpdates $ \e s -> case u e s of
            Nothing -> u' e s
            Just s' -> Just $ fromMaybe s' $ u' e s'
instance Monoid TrackerStateUpdates where
    mempty = TrackerStateUpdates $ \_ _ -> Nothing
instance (Given TrackerExtras) => Patch TrackerStateUpdates where
    type PatchTarget TrackerStateUpdates = TrackerState
    apply (TrackerStateUpdates u) = u given

getItemState :: ItemName -> TrackerExtras -> TrackerState -> ItemState
getItemState i e s = fromMaybe def $ s ^? stateItems . ifolded . indices (match i) where
        match i i' = i == i' || (i',i) `elem` e ^. itemAliases
getItemCount :: ItemName -> TrackerExtras -> TrackerState -> Int
getItemCount i e s = view itemStateAmount $ getItemState i e s

setItemCount :: ItemName -> Int -> TrackerStateUpdates
setItemCount i c = trackerStateUpdates $ \e s ->
        let s' = getItemState i e s
            c' = s' ^. itemStateAmount
        in groupLogs (LogSetItemCount i c c') $ setItemCount' i c
setItemCount' :: ItemName -> Int -> TrackerStateUpdates
setItemCount' i c = TrackerStateUpdates $ \e s ->
        let s' = getItemState i e s
            c' = s' ^. itemStateAmount
            i' = e ^.. itemAliases . folded . filtered ((== i) . snd) . _1
        in if c == c' then Nothing else case i' of
            [] -> Just $ s & stateItems . at i %~ \s' -> Just (fromMaybe def s' & itemStateAmount .~ c)
            aliases -> Just $ foldr (\i -> stateItems . at i %~ \s' -> Just (fromMaybe def s' & itemStateAmount .~ c)) s aliases

orElse :: TrackerStateUpdates -> TrackerStateUpdates -> TrackerStateUpdates
orElse (TrackerStateUpdates u) (TrackerStateUpdates u') = TrackerStateUpdates $ \e s -> u e s <|> u' e s

andThen :: TrackerStateUpdates -> TrackerStateUpdates -> TrackerStateUpdates
andThen (TrackerStateUpdates u) (TrackerStateUpdates u') = TrackerStateUpdates $ \e s -> u e s >>= \s' -> u' e s' <|> return s'

trackerStateUpdates :: (TrackerExtras -> TrackerState -> TrackerStateUpdates) -> TrackerStateUpdates
trackerStateUpdates u = TrackerStateUpdates $ \e s -> applyTrackerStateUpdates (u e s) e s

incrementItem, decrementItem, incrementItemWrapping, decrementItemWrapping :: ItemName -> TrackerStateUpdates
incrementItem i = groupLogs (LogIncrementItem i) $ trackerStateUpdates $ \e s ->
        let maxCount = getItemMaxCount i e
            c = getItemCount i e s
        in setItemCount' i (min (succ c) maxCount)
decrementItem i = groupLogs (LogDecrementItem i) $ trackerStateUpdates $ \e s ->
        let c = getItemCount i e s
        in setItemCount' i (max (pred c) 0)
incrementItemWrapping i = incrementItem i `orElse` setItemCount i 0
decrementItemWrapping i = trackerStateUpdates $ \e s ->
        let maxCount = getItemMaxCount i e
        in decrementItem i `orElse` setItemCount i maxCount

incrementComboWrapping :: ItemName -> TrackerStateUpdates
incrementComboWrapping i = groupLogs (LogSummary "Incremented Combo Item") $ trackerStateUpdates $ \e s ->
        case e ^? extrasComboItems . ix i . comboType of
            Nothing -> mempty
            Just t -> incrementComboWrapping' t i
incrementCombo :: ItemName -> TrackerStateUpdates
incrementCombo i = groupLogs (LogSummary "Incremented Combo Item") $ trackerStateUpdates $ \e s ->
        case e ^? extrasComboItems . ix i . comboType of
            Nothing -> mempty
            Just t -> incrementCombo' t i
incrementComboWrapping' :: ComboType -> ItemName -> TrackerStateUpdates
incrementComboWrapping' SimpleCombo i = trackerStateUpdates $ \e s ->
        let items = e ^. extrasComboItems . ix i . comboItems
        in incrementCombo' SimpleCombo i `orElse` foldMap (`setItemCount` 0) items
incrementComboWrapping' ProductCombo i = trackerStateUpdates $ \e s ->
        let items = e ^. extrasComboItems . ix i . comboItems
        in foldr (\i u -> incrementItem i `orElse` (setItemCount i 0 <> u)) mempty items
incrementCombo' :: ComboType -> ItemName -> TrackerStateUpdates
incrementCombo' SimpleCombo i = trackerStateUpdates $ \e s ->
        let items = e ^. extrasComboItems . ix i . comboItems
        in foldr (\i u -> incrementItem i `orElse` u) mempty items
incrementCombo' ProductCombo i = trackerStateUpdates $ \e s ->
        let items = e ^. extrasComboItems . ix i . comboItems
            isMax i = getItemCount i e s == getItemMaxCount i e
        in if all isMax items then mempty else incrementComboWrapping' ProductCombo i

setVisitedLocation :: LocationName -> Bool -> TrackerStateUpdates
setVisitedLocation l b' = groupLogs (bool (LogUnmarkLocation l) (LogMarkLocation l) b') $ trackerStateUpdates $ \e s ->
        let locationState = fromMaybe def $ s ^. stateLocations . at l
            b = locationState ^. locationStateVisited
            i = locationState ^. locationStateItem
            setLocation = TrackerStateUpdates $ \e s -> Just $ s & stateLocations . at l .~ Just (locationState & locationStateVisited .~ b')
        in if b == b' then mempty else setLocation <> maybe mempty (bool incrementItem decrementItem b) i
toggleVisitedLocation :: LocationName -> TrackerStateUpdates
toggleVisitedLocation l = trackerStateUpdates $ \e s ->
        let b = fromMaybe False $ s ^? stateLocations . ix l . locationStateVisited
        in setVisitedLocation l $ not b
setLocationItem :: LocationName -> Maybe ItemName -> TrackerStateUpdates
setLocationItem l i = trackerStateUpdates $ \e s ->
        let locationState = fromMaybe def $ s ^. stateLocations . at l
            b = locationState ^. locationStateVisited
            i' = locationState ^. locationStateItem
        in groupLogs (LogSetItemAtLocation i l i') $ setLocationItem' l i `andThen` (if b then maybe mempty decrementItem i' <> maybe mempty incrementItem i else mempty)
setLocationItem' :: LocationName -> Maybe ItemName -> TrackerStateUpdates
setLocationItem' l i = trackerStateUpdates $ \e s ->
        let locationState = fromMaybe def $ s ^. stateLocations . at l
            i' = locationState ^. locationStateItem
            setLocation = TrackerStateUpdates $ \e s -> Just $ s & stateLocations . at l .~ Just (locationState & locationStateItem .~ i)
        in if i == i' then mempty else setLocation
cyclePrize :: LocationName -> [ItemName] -> TrackerStateUpdates
cyclePrize l prizes = trackerStateUpdates $ \e s ->
        let nextItem = case (dropWhile (/= (s ^? stateLocations . ix l . locationStateItem . _Just)) $ Nothing : fmap Just prizes) of
                    [] -> Nothing
                    (_:[]) -> Nothing
                    (_:i:_) -> i
        in setLocationItem l nextItem

autoAssignItem :: TrackerStateUpdates
autoAssignItem = TrackerStateUpdates $ \e s ->
    autoAssignItem' s where
        autoAssignItem' s = case LogAutoAssignItemToLocation <$> mostRecentItem (s ^. stateLog) <*> mostRecentLocation s (flattenGroups (s ^. stateLog)) of
            Just l'@(LogAutoAssignItemToLocation i l) -> Just $ s & stateLog %~ (l':) & stateLocations . at l %~ (\s' -> Just $ fromMaybe def s' & locationStateItem .~ (Just i))
            Nothing -> Nothing
        mostRecentItem ((LogIncrementItem i):ls) = Just i
        mostRecentItem ((LogGroup l ls):lss) = mostRecentItem $ ls <> maybe [] (:[]) l <> lss --go through body of group first for combo items
        mostRecentItem ((LogAutoAssignItemToLocation _ _):_) = Nothing
        mostRecentItem (_:ls) = mostRecentItem ls
        flattenGroups ((LogGroup l ls):lss) = flattenGroups ls <> flattenGroups lss
        flattenGroups (l:ls) = l:flattenGroups ls
        flattenGroups [] = []
        mostRecentLocation s [] = Nothing
        mostRecentLocation s ((LogMarkLocation l):ls) = case s ^? stateLocations . ix l . locationStateItem . _Just of
            Nothing -> Just l
            Just _ -> mostRecentLocation s ls
        mostRecentLocation s ((LogUnmarkLocation l):ls) = mostRecentLocation s $ filter (/= (LogMarkLocation l)) ls
        mostRecentLocation s (_:ls) = mostRecentLocation s ls

setScope :: Scope -> TrackerStateUpdates
setScope scope = TrackerStateUpdates $ \e s ->
        let scope' = s ^. stateCurrentScope
        in if scope == scope' then Nothing else Just $ s & stateCurrentScope .~ scope & stateSelectedLocations .~ mempty

setSelection :: Set (Either Scope LocationName) -> TrackerStateUpdates
setSelection sel = TrackerStateUpdates $ \e s ->
        let sel' = s ^. stateSelectedLocations
        in maybe (if sel == sel' then Nothing else Just $ s & stateSelectedLocations .~ sel) (\scope -> applyTrackerStateUpdates (setScope scope) e s) $ sel ^? _Singleton . _Left
addSelection :: Set (Either Scope LocationName) -> TrackerStateUpdates
addSelection sel = TrackerStateUpdates $ \e s ->
        let sel' = s ^. stateSelectedLocations
        in if Set.isSubsetOf sel sel' then Nothing else Just $ s & stateSelectedLocations <>~ sel
removeSelection :: Set (Either Scope LocationName) -> TrackerStateUpdates
removeSelection sel = TrackerStateUpdates $ \e s ->
        let sel' = s ^. stateSelectedLocations
        in if Set.disjoint sel sel' then Nothing else Just $ s & stateSelectedLocations %~ (Set.\\ sel)

diffSelection :: Set (Either Scope LocationName) -> TrackerStateUpdates
diffSelection sel = trackerStateUpdates $ \e s ->
        let sel' = s ^. stateSelectedLocations
        in if Set.isSubsetOf sel sel' then removeSelection sel else addSelection sel

markSelectedSatisfying :: Map LocationName Bool -> TrackerStateUpdates
markSelectedSatisfying condition = groupMarkLocations $ trackerStateUpdates $ \e s ->
        getDual $ s ^. stateSelectedLocations . folded . _Right . filtered (\l -> (condition ^. at l) == Just True) . to (\l -> Dual $ setVisitedLocation l True)
markSelected :: TrackerStateUpdates
markSelected = groupMarkLocations $ trackerStateUpdates $ \e s ->
        getDual $ s ^. stateSelectedLocations . folded . _Right . to (\l -> Dual $ setVisitedLocation l True)
unmarkSelected :: TrackerStateUpdates
unmarkSelected = groupUnmarkLocations $ trackerStateUpdates $ \e s ->
        getDual $ s ^. stateSelectedLocations . folded . _Right . to (\l -> Dual $ setVisitedLocation l False)

getComboDisplayCount :: ComboType -> TrackerExtras -> [(ItemState, ItemName)] -> Int
getComboDisplayCount SimpleCombo extras = sumOf (each . _1 . itemStateAmount)
getComboDisplayCount ProductCombo extras = foldr (\(a, ((`getItemMaxCount` extras) -> b)) c -> a + (succ b * c)) 0

addLog :: TrackerStateUpdateLog -> [TrackerStateUpdateLog] -> [TrackerStateUpdateLog]
addLog l ((LogGroup Nothing ls):lss) = LogGroup Nothing (addLog l ls):lss
addLog l ls = l:ls

writeLog :: TrackerStateUpdateLog -> TrackerStateUpdates -> TrackerStateUpdates
writeLog l u = TrackerStateUpdates $ \e s ->
    case applyTrackerStateUpdates u e (s & stateLog %~ (LogGroup Nothing []:)) of
        Nothing -> Nothing
        Just s' -> maybe (Just s') Just $ closeGroup l s' where
            closeGroup l s = mapMOf stateLog (closeGroup' l) s
            closeGroup' l ((LogGroup Nothing []):lss) = Just $ addLog l lss
            closeGroup' l ((LogGroup Nothing ls):lss) = case closeGroup' l ls of
                                                        Nothing -> Just $ addLog (LogGroup (Just l) ls) lss
                                                        Just ls' -> Just $ LogGroup Nothing ls' : lss
            closeGroup' _ _ = Nothing

groupLogs :: TrackerStateUpdateLog -> TrackerStateUpdates -> TrackerStateUpdates
groupLogs l u = writeLog l u `andThen` clearRedo where
    clearRedo = TrackerStateUpdates $ \e s ->
        Just $ s & stateRedo .~ Nothing

groupSummarize :: ([TrackerStateUpdateLog] -> Text) -> TrackerStateUpdates -> TrackerStateUpdates
groupSummarize d u = groupLogs (LogSummary "") u <> summarize where
    summarize = TrackerStateUpdates $ \e s ->
        mapMOf stateLog summarize' s
    summarize' ((LogGroup Nothing ls):lss) = case summarize' ls of
        Nothing -> Nothing
        Just ls' -> Just (LogGroup Nothing ls' : lss)
    summarize' ((LogGroup (Just (LogSummary "")) ls):lss) = Just $ LogGroup (Just (LogSummary (d ls))) ls : lss
    summarize' (LogSummary "":lss) = Just $ LogSummary (d []) : lss
    summarize' _ = Nothing

groupMarkLocations :: TrackerStateUpdates -> TrackerStateUpdates
groupMarkLocations = groupSummarize countMarks where
    countMarks ls = "Marked " <> pack (show (countMarks' 0 ls)) <> " locations"
    countMarks' n [] = n
    countMarks' n ((LogMarkLocation _):ls) = n `seq` countMarks' (succ n) ls
    countMarks' n ((LogGroup l ls):lss) = countMarks' n (maybe [] (:[]) l <> ls <> lss)
    countMarks' n (_:ls) = countMarks' n ls

groupUnmarkLocations :: TrackerStateUpdates -> TrackerStateUpdates
groupUnmarkLocations = groupSummarize countUnmarks where
    countUnmarks ls = "Unmarked " <> pack (show (countUnmarks' 0 ls)) <> " locations"
    countUnmarks' n [] = n
    countUnmarks' n ((LogUnmarkLocation _):ls) = n `seq` countUnmarks' (succ n) ls
    countUnmarks' n ((LogGroup l ls):lss) = countUnmarks' n (maybe [] (:[]) l <> ls <> lss)
    countUnmarks' n (_:ls) = countUnmarks' n ls

canUndo :: TrackerState -> Bool
canUndo = view $ stateLog . to (not . null)

canRedo :: TrackerState -> Bool
canRedo = view $ stateRedo . to (not . null)

redo :: TrackerStateUpdates
redo = TrackerStateUpdates $ \e s ->
    s ^. stateRedo

undo :: TrackerStateUpdates
undo = trackerStateUpdates $ \e s ->
    undoAction s `andThen` addRedo s where
        undoAction s = case s ^? stateLog . folded of
            Nothing -> mempty
            Just a -> removeNewestLog `andThen` groupLogs (LogSummary "Undo") (undoAction' a) `andThen` removeUndoFromLog
        undoAction' (LogIncrementItem i) = decrementItem i
        undoAction' (LogDecrementItem i) = incrementItem i
        undoAction' (LogSetItemCount i c c') = setItemCount i c'
        undoAction' (LogMarkLocation l) = setVisitedLocation l False
        undoAction' (LogUnmarkLocation l) = setVisitedLocation l True
        undoAction' (LogSetItemAtLocation i l i') = setLocationItem l i'
        undoAction' (LogAutoAssignItemToLocation i l) = setLocationItem' l Nothing
        undoAction' (LogSummary _) = mempty
        undoAction' (LogGroup (Just l) ls) = undoAction' l `orElse` foldMap undoAction' ls
        removeNewestLog = TrackerStateUpdates $ \e s ->
            Just $ s & stateLog %~ tail
        removeUndoFromLog = TrackerStateUpdates $ \e s ->
            Just $ s & stateLog %~ removeUndoFromLog'
        removeUndoFromLog' (LogSummary "Undo":ls) = ls
        removeUndoFromLog' (LogGroup (Just (LogSummary "Undo")) _ : ls) = ls
        removeUndoFromLog' log = error $ "Unexpected Log " <> show log
        addRedo s = TrackerStateUpdates $ \e s' ->
            Just $ s' & stateRedo .~ Just s
