{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Reflex.Dom

import Control.Monad

import Control.Monad.IO.Class (liftIO)

import Control.Monad.Choice

import Data.Map (Map())
import qualified Data.Map as Map
import Data.Set (Set())
import qualified Data.Set as Set
import Data.Map.Monoidal (MonoidalMap())
import qualified Data.Map.Monoidal as MM
import Data.Text (Text(), pack, unpack)
import Data.Bool (bool)
import Text.Read (readMaybe)
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Foldable (fold)
import Data.Void (absurd)
import Control.Monad.Fix

import Data.FileEmbed

import Reflex.Dom.Image
import qualified Language.Javascript.JSaddle as J

import Control.Lens
import Control.Lens.TH

import Data.Reflection

import Reflex.Dom.ChoiceT

import Tracker
import Tracker.Display
import Tracker.Logic
import Tracker.Builtin

headWidget :: Widget x ()
headWidget = do
    el "title" $ text "The Minish Cap Randomizer Tracker"
    elAttr "meta" ("name" =: "viewport" <> "content" =: "width=device-width, initial-scale=1.0") blank
    elAttr "link" ("rel" =: "stylesheet" <> "type" =: "text/css" <> "href" =: (fileURI "text/css" $(embedFile "resources/css/style.css"))) blank
    elAttr "link" ("rel" =: "icon" <> "type" =: "image/png" <> "href" =: (toURI $$(staticImage "resources/images/favicon.png"))) blank

main :: IO ()
main = mainWidgetWithHead headWidget $ mdo
    (undoEvent, redoEvent) <- do
        let undoAttr b = "type" =: "button"
                    <> "class" =: "undo-button"
                    <> "value" =: "Undo"
                    <> bool ("disabled" =: "") mempty b
            redoAttr b = "type" =: "button"
                    <> "class" =: "redo-button"
                    <> "value" =: "Redo"
                    <> bool ("disabled" =: "") mempty b
        (undoE, _) <- elDynAttr' "input" (undoAttr <$> fromUniqDynamic (canUndo <$> trackerState)) $ blank
        (redoE, _) <- elDynAttr' "input" (redoAttr <$> fromUniqDynamic (canRedo <$> trackerState)) $ blank
        return (fmap (const undo) $ domEvent Click undoE, fmap (const redo) $ domEvent Click redoE)
    (tracker, mapScale) <- mdo
        shown <- toggle False $ domEvent Click e
        let attr = fmap (\b -> "class" =: ("settings_container" <>  bool " hidden" "" b)) shown
        -- savedSettings <- (>>= readMaybe . unpack ) <$> getFromStorage "settings"
        let defaultSettings = mempty --fromMaybe mempty savedSettings
        settings <- foldDynMaybe apply defaultSettings settingsChanges
        (tracker, queries) <- flip runQueryT settings $ execChoiceT $ getTracker $ Builtin TMCR
        (settingsChanges, e, mapScale) <- elDynAttr "div" attr $ do
            (e, _) <- elClass' "div" "settingsToggleShown" blank
            mapScale <- do
                elAttr "label" ("for" =: "map-scale") $ text "Map Scale:"
                r <- rangeInput $ def &
                    rangeInputConfig_initialValue .~ 1 &
                    rangeInputConfig_attributes .~ pure ("id" =: "map-scale" <> "min" =: "0.2" <> "max" =: "2" <> "step" =: "0.01")
                return $ uniqDynamic $ _rangeInput_value r
            queryKeys <- holdUniqDyn $ fmap (MM.keysSet . getUserChoiceQuery) $ incrementalToDynamic queries
            settingsChanges' <- elClass "div" "settings" $ dyn $ ffor queryKeys $ \q -> do
                currentSettings <- Map.mapMaybe getFirst <$> sample (current settings)
                settingsChanges' <- fmap (mergeWith (<>)) $ forM (Set.toList q) $ \(i, k) -> do
                    dynV <- holdUniqDyn $ fmap ((MM.! (i, k)) . getUserChoiceQuery) $ incrementalToDynamic queries
                    let hiddenClass = bool "" " hidden" . (== mempty) <$> dynV
                    case k of
                        UserChoiceOption key options ->
                            elDynClass "div" ((("settings_dropdown_" <> key) <>) <$> hiddenClass) $ do
                                let opts = pure $ Map.fromAscList . itoList $ options
                                el <- dropdown (Map.findWithDefault 0 key currentSettings) opts $ def
                                return $ fmap (PatchMap . (key =:) . Just . First . Just) $ el ^. dropdown_change
                        UserChoiceFlag key label defaultValue ->
                            elDynClass "div" ((("settings_checkbox_" <> key) <>) <$> hiddenClass) $ do
                                let initialVal = maybe defaultValue (/= 0) (Map.lookup key currentSettings)
                                rec checked <- toggle initialVal $ domEvent Click slider
                                    (slider, _) <- elDynAttr "span" (fmap (\b -> "class" =: ("switch" <> bool "" " checked" b)) checked) $ (text "\x200B" >> elClass' "span" "slider" blank)
                                elAttr "span" ("class" =: "label") $ text label
                                return $ fmap (PatchMap . (key =:) . Just . First . Just . bool 0 1) $ updated checked
                        UserChoiceNumber key label ->
                            elDynClass "div" ((("settings_numberbox_" <> key) <>) <$> hiddenClass) $ do
                                input <- inputElement $ def
                                    & inputElementConfig_elementConfig . elementConfig_initialAttributes .~ mapKeysToAttributeName ("type" =: "number" <> "placeholder" =: label)
                                    & inputElementConfig_initialValue .~ fromMaybe "" (fmap (pack . show) (Map.lookup key currentSettings))
                                return $ fmap (PatchMap . (key =:) . Just . First . Just) $ mapMaybe (readMaybe @Int . unpack) $ updated $ _inputElement_value input
            settingsChanges <- switchHold never settingsChanges'
            return (settingsChanges, e, mapScale)
        return (tracker, mapScale)
    trackerState <- accumMaybe (&) def changes
    changes <- elClass "div" "tracker-container" $ do
        changes' <- dyn $ ffor tracker $ \(tracker, disp) -> do
            stateChanges <- displayTracker tracker disp mapScale trackerState
            return $ give (tracker ^. trackerExtras) $ fmap apply $ stateChanges <> undoEvent <> redoEvent
        switchHold never changes'
    return ()

displayTracker :: (PostBuild t m, MonadHold t m, MonadFix m, DomBuilder t m) => Tracker -> TrackerDisplayData -> UniqDynamic t Float -> UniqDynamic t TrackerState -> m (Event t TrackerStateUpdates)
displayTracker tracker disp mapScale state = mdo
    let scope = fmap (view stateCurrentScope) state
    ((), ev) <- runEventWriterT $ do
        let relevantItems = (tracker ^. (trackerExtras . nonLogicTrackedItems . each . to Set.singleton) <..> (trackerLogic . each . ruleBody . traverse . _Left . to Set.singleton)) Set.\\ (tracker ^. trackerExtras . untrackedItems . each . _1 . to Set.singleton)
            relevantLocations = tracker ^. trackerLogic . each . ruleHead . to Set.singleton
            extras = tracker ^. trackerExtras
            items' = fmap pure (tracker ^. trackerExtras . untrackedItems . to Map.fromList)
                  <> Map.fromSet (\i -> fmap (getItemState i extras) state) relevantItems
            locations' = Map.fromSet (\l -> fmap (Map.findWithDefault def l . view stateLocations) state) relevantLocations
            reachable = buildTracker items' (tracker ^. trackerLogic)
            -- spheres = fmap (>>= traverse getSphereCount) $ countSpheres (tracker ^. trackerLogic) (fmap (fmap (view locationStateItem)) locations')
        currentLocationStates <- traverse (sample . current . fromUniqDynamic) locations'
        sphereState <- accum (flip updateSphereState) (updateSphereState [(l, Just i) | (l, s) <- Map.toList currentLocationStates, Just i <- [s ^. locationStateItem]] $ initialSphereState (tracker ^. trackerExtras . untrackedItems) (tracker ^. trackerLogic)) (fmap Map.toList $ mergeMap $ fmap (updated . fromUniqDynamic . fmap (view locationStateItem)) locations')
        let spheres' = fmap (fmap (\case Sphere i -> Just i; UnknownSphere -> Nothing) . getSpheres) sphereState
            spheres = Map.fromSet (\l -> fmap (Map.findWithDefault def l) spheres') relevantLocations
        elClass "div" "tracker-always" $ do
            forM (tracker ^. trackerInterface . alwaysWidget) $ \w -> do
                displayTrackerWidget w tracker disp mapScale items' locations' reachable spheres
            displayLocationSelection tracker disp (view stateSelectedLocations <$> state) items' locations' reachable spheres
        elClass "div" "tracker-sub-trackers" $
            displaySubTrackers tracker disp mapScale scope items' locations' reachable spheres
        return ()
    return ev

lookupItem :: [(ItemName, ItemName)] -> ItemName -> Map ItemName ItemState -> ItemState
lookupItem aliases i items = case filter ((== i) . snd) aliases of
    [] -> Map.findWithDefault def i items
    as -> maximum $ fmap (\(i',_) -> Map.findWithDefault 0 i' items) as

data LocationsDisplayState = AllVisited | NoneReachable | SomeReachable | AllReachable
        deriving (Eq, Ord, Show, Enum)
instance Semigroup LocationsDisplayState where
    AllVisited <> x = x
    x <> AllVisited = x
    NoneReachable <> NoneReachable = NoneReachable
    AllReachable <> AllReachable = AllReachable
    _ <> _ = SomeReachable
instance Monoid LocationsDisplayState where
    mempty = AllVisited

locationDisplayState :: (Applicative f) => Map LocationName (f LocationState) -> Map LocationName (f Bool) -> LocationName -> f LocationsDisplayState
locationDisplayState locations reachable l = (\ls r -> if ls ^. locationStateVisited then AllVisited else if r then AllReachable else NoneReachable) <$> Map.findWithDefault (pure def) l locations <*> Map.findWithDefault (pure False) l reachable


displayLocationSelection :: forall t m. (MonadHold t m, PostBuild t m, DomBuilder t m, EventWriter t TrackerStateUpdates m) => Tracker -> TrackerDisplayData -> UniqDynamic t (Set (Either Scope LocationName)) -> Map ItemName (UniqDynamic t ItemState) -> Map LocationName (UniqDynamic t LocationState) -> Map LocationName (UniqDynamic t Bool) -> Map LocationName (UniqDynamic t (Maybe Int)) -> m ()
displayLocationSelection tracker disp selection items locations reachable spheres = elDynClass "div" (fromUniqDynamic $ ffor selection $ \sel -> "selected-locations-view" <> bool "" " empty-selection" (null sel)) $ do
    let summary :: UniqDynamic t LocationsDisplayState
        summary = do
            sel <- selection
            ds <- forM (sel ^.. folded . _Right) $ \l -> locationDisplayState locations reachable l
            return $ fold ds
        attrClearSelection sel = ("class" =: "clear-selection"
                               <> "type" =: "button"
                               <> "value" =: "Clear selection"
                               <> bool mempty ("disabled" =: "") (null sel))
        attrMarkSelected sel summary = ("class" =: "mark-selection"
                                     <> "type" =: "button"
                                     <> "value" =: (case summary of AllVisited -> "Unmark all"; SomeReachable -> "Mark all available"; _ -> "Mark all")
                                     <> bool mempty ("disabled" =: "") (null sel))
    (clearEl, markEl) <- elClass "div" "button-row" $ do
        (clearEl,_) <- elDynAttr' "input" (attrClearSelection <$> fromUniqDynamic selection) blank
        (markEl, _) <- elDynAttr' "input" (attrMarkSelected <$> fromUniqDynamic selection <*> fromUniqDynamic summary) blank
        return (clearEl, markEl)
    evs <- elClass "div" "locations-list" $ dyn $ ffor (fromUniqDynamic selection) $ \sel -> do
        evs <- forM (sel ^.. folded) $ \l -> do
            displayLocationListEntry tracker disp l items locations reachable spheres
        return $ mergeWith (<>) evs
    ev <- switchHold never evs
    tellEvent ev
    tellEvent $ fmap (const (setSelection mempty)) $ domEvent Click clearEl
    tellEvent $ fmap (\reachable' -> (markSelectedSatisfying reachable' `orElse` markSelected `orElse` unmarkSelected)) $ tag (current (traverse fromUniqDynamic reachable)) $ domEvent Click markEl
    

displayLocationListEntry :: forall t m. (DomBuilder t m, PostBuild t m) => Tracker -> TrackerDisplayData -> Either Scope LocationName -> Map ItemName (UniqDynamic t ItemState) -> Map LocationName (UniqDynamic t LocationState) -> Map LocationName (UniqDynamic t Bool) -> Map LocationName (UniqDynamic t (Maybe Int)) -> m (Event t TrackerStateUpdates)
displayLocationListEntry tracker disp (Left scope) items locations reachable spheres = do
    (e, _)  <- elClass' "div" "location-list-entry location-list-entry-scope" $ do
        displayImage $ disp ^. scopeIcon
        elClass "span" "scope-name" $ text $ displayScope scope
        elClass "span" "scope-summary" $ do
            let locs = tracker ^. trackerInterface . subTrackers . ix scope . folded . to handledLocations
            (displaySummary locs locations reachable) :: m ()
        return () :: m ()
    return $ fmap (const (setScope scope)) $ domEvent Click e
displayLocationListEntry tracker disp (Right location) items locations reachable spheres = do
    (e, _) <- elClass' "div" "location-list-entry location-list-entry-location" $ do
        let d = locationDisplayState locations reachable location
            attrClass d = ("location-name" <> case d of
                                AllVisited -> " visited"
                                AllReachable -> " reachable"
                                NoneReachable -> " unreachable")
            s = Map.findWithDefault (pure def) location locations
            withItemState = do
                s' <- s
                case s' ^. locationStateItem of
                    Nothing -> return Nothing
                    Just itemName -> do
                        itemState <- Map.findWithDefault (pure def) itemName items
                        return $ Just (s' ^. locationStateVisited, itemName, itemState)
        elClass "span" "location-sphere" $ dynText $ fmap (\case Nothing -> "[?]"; Just n -> "[" <> pack (show n) <> "]") $ fromUniqDynamic $ Map.findWithDefault (pure Nothing) location spheres
        elDynClass "span" (attrClass <$> fromUniqDynamic d) $ text location
        elClass "div" "location-item" $ dyn_ $ ffor (fromUniqDynamic withItemState) $ \s -> fromMaybe blank $ s >>= \(visitedLocation, itemName, itemState) ->
            fmap displayImage $ bool getNextImage getCurrentImage visitedLocation disp itemName (itemState ^. itemStateAmount)
        return () :: m ()
    return $ fmap (const (toggleVisitedLocation location)) $ domEvent Click e
        
        
displaySummary :: (DomBuilder t m, PostBuild t m, Foldable f) => f LocationName -> Map LocationName (UniqDynamic t LocationState) -> Map LocationName (UniqDynamic t Bool) -> m ()
displaySummary locs locationStates reachable = do
    let andCount AllVisited = ((mempty, mempty, Sum 1), AllVisited)
        andCount NoneReachable = ((mempty, Sum 1, mempty), NoneReachable)
        andCount AllReachable = ((Sum 1, mempty, mempty), AllReachable)
        summary = getAp $ foldMap (\l -> Ap $ fmap andCount $ locationDisplayState locationStates reachable l) locs
    elClass "span" "summary-not-visited" $ dyn_ $ ffor (fromUniqDynamic summary) $ \((a,b,_),s) -> case s of
        AllVisited -> blank
        AllReachable -> elClass "span" "summary-reachable" $ text $ (<> " ") $ pack $ show $ getSum $ a
        NoneReachable -> elClass "span" "summary-unreachable" $ text $ (<> " ") $ pack $ show $ getSum $ b
        SomeReachable -> do
            elClass "span" "summary-reachable" $ text $ pack $ show $ getSum $ a
            text " + "
            elClass "span" "summary-unreachable" $ text $ pack $ show $ getSum $ b
            text " "
    elClass "span" "summary-total" $ dynText $ fmap (\((a,b,c),_) -> "(" <> pack (show (getSum (a <> b <> c))) <> ")") $ fromUniqDynamic summary

displayScope :: Scope -> Text
displayScope = foldr (\a b -> "/" <> a <> b) "/"

displaySubTrackers :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m, EventWriter t TrackerStateUpdates m) => Tracker -> TrackerDisplayData -> UniqDynamic t Float -> UniqDynamic t Scope -> Map ItemName (UniqDynamic t ItemState) -> Map LocationName (UniqDynamic t LocationState) -> Map LocationName (UniqDynamic t Bool) -> Map LocationName (UniqDynamic t (Maybe Int)) -> m ()
displaySubTrackers tracker disp mapScale scope items locations reachable spheres = do
    let scopeList = itoList $ tracker ^. trackerInterface . subTrackers
        opts = Map.fromList $ fmap (\(scope, _) -> (scope, displayScope scope)) $ scopeList
    ev' <- dyn $ ffor (fromUniqDynamic scope) $ \currentScope -> elClass "div" "scope-selector" $ do
        (b, _) <- elAttr' "input" ("type" =: "button" <> "class" =: "scope-parent-button" <> bool mempty ("disabled" =: "") (currentScope == []) <> "value" =: "\x21B0") blank
        d <- dropdown currentScope (pure opts) $ def
        let buttonChange = if null currentScope then mempty else const (init currentScope) <$> domEvent Click b
        return $ buttonChange <> _dropdown_change d
    ev <- switchHold never ev'
    tellEvent $ fmap setScope $ ev
    forM_ scopeList $ \(widgetScope, widgets) -> do
        let shown currentScope = currentScope == widgetScope
            attrClass currentScope = "tracker-scope" <> bool " hidden" "" (shown currentScope)
        elDynClass "div" (attrClass <$> fromUniqDynamic scope) $ forM_ widgets $ \widget ->
            displayTrackerWidget widget tracker disp mapScale items locations reachable spheres

displayTrackerWidget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m, EventWriter t TrackerStateUpdates m) => TrackerWidget -> Tracker -> TrackerDisplayData -> UniqDynamic t Float -> Map ItemName (UniqDynamic t ItemState) -> Map LocationName (UniqDynamic t LocationState) -> Map LocationName (UniqDynamic t Bool) -> Map LocationName (UniqDynamic t (Maybe Int)) -> m ()
displayTrackerWidget (TrackerWidgetMap l) tracker disp mapScale items locations reachable spheres =
    let attr s = ("class" =: "tracker-widget tracker-widget-map" <> "style" =: ("transform: scale(" <> pack (show s) <> "); transform-origin: 0 0;"))
    in elDynAttr "div" (attr <$> fromUniqDynamic mapScale) $
        displayMapWidget l tracker disp items locations reachable
displayTrackerWidget (TrackerWidgetGrid g) tracker disp mapScale items locations reachable spheres =
    elClass "div" "tracker-widget tracker-widget-grid" $
        iforM_ g $ \y row -> iforM_ row $ \x p ->
            elAttr "div" ("class" =: "tracker-item"
                       <> "style" =: ("grid-column-start: " <> pack (show (x + 1)) <> "; "
                                   <> "grid-row-start: "    <> pack (show (y + 1)) <> ";")) $
                displayTrackerWidgetPrimitive disp (tracker ^. trackerExtras) items locations p
displayTrackerWidget (TrackerWidgetLocationList locs) tracker disp mapScale items locations reachable spheres =
    elClass "div" "tracker-widget tracker-widget-location-list" $ do
        evs <- forM locs $ \l ->
            displayLocationListEntry tracker disp l items locations reachable spheres
        tellEvent $ mergeWith (<>) evs

displayMapWidget :: (MonadFix m, MonadHold t m, PostBuild t m, DomBuilder t m, EventWriter t TrackerStateUpdates m) => LocationMap -> Tracker -> TrackerDisplayData -> Map ItemName (UniqDynamic t ItemState) -> Map LocationName (UniqDynamic t LocationState) -> Map LocationName (UniqDynamic t Bool) -> m ()
displayMapWidget (LocationMap area) tracker disp items locations reachable =
    elClass "div" "location-map location-map-simple" $
        displayArea area tracker disp items locations reachable
displayMapWidget (LocationMapNested img width height regions) tracker disp items locations reachable =
    elClass "div" "location-map location-map-nested" $ do
        let attr = ("class" =: "location-map-thumbnail"
                 <> "style" =: ("background: url(" <> toURI img <> "); "
                             <> "width: "  <> pack (show width)  <> "px; "
                             <> "height: " <> pack (show height) <> "px;"))
        (selectArea, areas, (width, height)) <- elAttr "div" attr $ do
            areas <- forM (Map.assocs regions) $ \(name, (x,y,width,height,area)) -> do
                let attr = ("class" =: "location-map-thumbnail-area"
                         <> "style" =: ("left: " <> pack (show x) <> "px; "
                                     <> "top: " <> pack (show y) <> "px; "
                                     <> "width: " <> pack (show width) <> "px; "
                                     <> "height: " <> pack (show height) <> "px;"))
                (e, _) <- elAttr' "div" attr $
                    displaySummary (area ^.. locationMapPins . traverse . locationMapPinContents . traverse . _Right) locations reachable
                return (domEvent Click e, name, area)
            let f (ev, name, area) (evs, areas', (width, height)) = (fmap (const name) ev <> evs, 
                                                                     (name, area):areas',
                                                                     (max width ((area ^. locationMapAreaOffsetX) + (area ^. locationMapAreaWidth)), 
                                                                      max height ((area ^. locationMapAreaOffsetY) + (area ^. locationMapAreaHeight))))
            return $ foldr f (never,[], (0,0)) areas
        rec shownArea <- holdDyn Nothing (fmap Just selectArea <> fmap (const Nothing) resetArea)
            resetArea <- do
                let attr shownArea' = ("class" =: ("reset-area" <> bool "" " hidden" (null shownArea'))
                                    <> "type"  =: "button"
                                    <> "value" =: "Close")
                (e, _) <- elDynAttr' "input" (attr <$> shownArea) blank
                return $ domEvent Click e
        elAttr "div" ("class" =: "nested-areas-wrapper" <> "style" =: ("width: " <> pack (show width) <> "px; height: " <> pack (show height) <> "px;")) $
            forM_ areas $ \(name, area) ->
                elDynClass "div" ((\shownArea' -> "nested-area-wrapper" <> bool " hidden-area" " shown-area" (shownArea' == Just name)) <$> shownArea) $
                    displayArea area tracker disp items locations reachable
        dyn_ $ ffor shownArea $ \case
            Nothing -> blank
            Just area -> elClass "span" "area-name" $ text area


displayArea :: (PostBuild t m, DomBuilder t m, EventWriter t TrackerStateUpdates m) => LocationMapArea -> Tracker -> TrackerDisplayData -> Map ItemName (UniqDynamic t ItemState) -> Map LocationName (UniqDynamic t LocationState) -> Map LocationName (UniqDynamic t Bool) -> m ()
displayArea area tracker disp items locations reachable = do
    let attr = ("class" =: "map-area"
             <> "style" =: ("left: " <> (area ^. locationMapAreaOffsetX . to show . to pack) <> "px; "
                         <> "top: " <> (area ^. locationMapAreaOffsetY . to show . to pack) <> "px; "
                         <> "width: " <> (area ^. locationMapAreaWidth . to show . to pack) <> "px; "
                         <> "height: " <> (area ^. locationMapAreaHeight . to show . to pack) <> "px; "
                         <> "background: url(" <> toURI (area ^. locationMapAreaImage) <> ");"))
    elAttr "div" attr $ elClass "div" "pin-box" $
        iforMOf_ (locationMapPins . itraversed) area $ \(x,y) pin -> do
            displayPin x y pin tracker disp items locations reachable

displayPin :: (PostBuild t m, DomBuilder t m, EventWriter t TrackerStateUpdates m) => Int -> Int -> LocationMapPin -> Tracker -> TrackerDisplayData -> Map ItemName (UniqDynamic t ItemState) -> Map LocationName (UniqDynamic t LocationState) -> Map LocationName (UniqDynamic t Bool) -> m ()
displayPin x y pin tracker disp items locations reachable = do
    let contents = pin ^. locationMapPinContents . folded . to (either (\s -> tracker ^. trackerInterface . subTrackers . ix s . folded . to handledLocations) Set.singleton)
        displayStatus = getAp $ foldMap (Ap . locationDisplayState locations reachable) contents
        attr d = ("class" =: ("pin" <> (case d of AllReachable -> " all-reachable"; NoneReachable -> " none-reachable"; SomeReachable -> " some-reachable"; AllVisited -> " all-visited"))
                 <> "style" =: ("left: " <> pack (show x) <> "px; "
                             <> "top: " <> pack (show y) <> "px; "))
    (e, _) <- elDynAttr' "div" (attr <$> fromUniqDynamic displayStatus) blank --todo display item
    tellEvent $ fmap (const $ setSelection $ pin ^. locationMapPinContents . to Set.fromList) $ domEvent Click e

displayTrackerWidgetPrimitive :: (MonadHold t m, PostBuild t m, DomBuilder t m, EventWriter t TrackerStateUpdates m) => TrackerDisplayData -> TrackerExtras -> Map ItemName (UniqDynamic t ItemState) -> Map LocationName (UniqDynamic t LocationState) -> TrackerWidgetPrimitive -> m ()
displayTrackerWidgetPrimitive disp extras items locations (SimpleItem i) = fromMaybe blank $ ffor (items ^? ix i . to fromUniqDynamic) $ \d -> do
        let maxCount = getItemMaxCount i extras
            showCount = maxCount > 1 && anyOf (itemProgressiveOverrides . at i . _Nothing) (const True) disp
            attr c = "class" =: "tracker-item tracker-simple-item" 
                   <> "data-count" =: pack (show c) 
                   <> "data-name" =: i 
                   <> (maybe mempty (\img -> "src" =: toURI img) $ getCurrentImage disp i $ c ^. itemStateAmount)
        (e, _) <- elDynAttr' "img" (attr <$> d) blank 
        if showCount 
            then elAttr "span" ("class" =: "tracker-count") $ dynText $ (\c -> pack $ show $ c ^. itemStateAmount) <$> d
            else return ()
        tellEvent $ fmap (\() -> incrementItemWrapping i `andThen` autoAssignItem) $ domEvent Click e
        --todo right click decrement
displayTrackerWidgetPrimitive disp extras items locations (SpecialLocation l prizes) = fromMaybe blank $ ffor (locations ^? ix l . to fromUniqDynamic) $ \d -> do
        let attr s = "class" =: "tracker-item tracker-special-location"
                   <> "data-count" =: (s ^. locationStateVisited . to fromEnum . to show . to pack)
                   <> "data-name" =: l
                   <> (maybe mempty (\img -> "src" =: toURI img) $ disp ^. specialLocationImages . at l)
        (i, _) <- elDynAttr' "img" (attr <$> d) blank
        let items' = traverse fromUniqDynamic $ Map.restrictKeys items $ Set.fromList prizes
        unless (null prizes) $ do
            (prizeElement, _) <- elClass' "div" "tracker-special-location-prize" $
                let attr s items = "class" =: "tracker-special-location-prize"
                                <> (maybe mempty (\i -> "data-name" =: i) $ s ^. locationStateItem)
                                <> "src" =: (fromMaybe mempty $ do
                                    i <- (s ^. locationStateItem)
                                    let b = s ^. locationStateVisited
                                    c <- Map.lookup i items
                                    img <- bool getNextImage getCurrentImage b disp i $ c ^. itemStateAmount
                                    return $ toURI img)
                in elDynAttr "img" (attr <$> d <*> items') $ text "?"
            tellEvent $ fmap (\() -> cyclePrize l prizes) $ domEvent Click prizeElement
        tellEvent $ fmap (\() -> toggleVisitedLocation l) $ domEvent Click i
displayTrackerWidgetPrimitive disp extras items locations (ComboItem comboName) = fromMaybe blank $ do
            comboData <- extras ^. extrasComboItems . at comboName
            values <- traverse (\name -> (, name) <$> items ^. at name) $ comboData ^. comboItems
            let d = fromUniqDynamic $ traverse (\(c, name) -> (, name) <$> c) values
            Just $ do
                let displayCount values = getComboDisplayCount (comboData ^. comboType) extras values
                    attr values = ("class" =: "tracker-item tracker-combo-item"
                                <> "data-count" =: pack (show (displayCount values))
                                <> "data-name" =: comboName
                                <> (maybe mempty (\img -> "src" =: toURI img) $ getCurrentImage disp comboName (displayCount values)))
                (i, _) <- elDynAttr' "img" (attr <$> d) blank
                tellEvent $ fmap (\() -> incrementCombo comboName `andThen` autoAssignItem) $ domEvent Click i
displayTrackerWidgetPrimitive disp extras items locations (Scope scope) = do
            let attr = ("class" =: "tracker-item tracker-select-scope"
                     <> "data-name" =: displayScope scope
                     <> "src" =: disp ^. scopeIcon . to toURI)
            (i, _) <- elAttr' "img" attr blank
            tellEvent $ fmap (\() -> setScope scope) $ domEvent Click i

getFromStorage :: (J.MonadJSM m) => Text -> m (Maybe Text)
getFromStorage label = J.liftJSM $ do
        v <- J.jsg "window" ^. J.js "sessionStorage" ^. J.js1 "getItem" label
        J.fromJSVal v

saveInStorage :: (J.MonadJSM m) => Text -> Maybe Text -> m ()
saveInStorage label value = J.liftJSM $ void $ J.jsg "window" ^. J.js "sessionStorage" ^. maybe (J.js1 "removeItem" label) (J.js2 "setItem" label) value

(<..>) :: Fold s a -> Fold s a -> Fold s a
(<..>) f g r s = fmap absurd $ (<>) <$> contramap absurd (f r s) <*> contramap absurd (g r s)
