module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import LogicParser
import Data.Logic
import Reactive.Logic

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe, maybe)
import Data.Bool (bool)
import Control.Monad (when, void, unless, forM)
import Control.Arrow (first, second)

import Data.Map (Map())
import qualified Data.Map as Map

import Control.Monad.Trans.Cont
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.State (execStateT, modify, StateT())

main :: IO ()
main = startGUI defaultConfig{ jsStatic = Just "./resources/"} setup

logicFiles :: NonEmpty (String, FilePath)
logicFiles = ("default", "resources/default.logic") :| []

setup :: Window -> UI ()
setup window = evalContT $ do
    lift $ return window # set UI.title "The Minish Cap Randomizer Tracker"
    lift $ UI.addStyleSheet window "style.css"
    lift $ runFunction $ ffi "document.addEventListener('contextmenu', function (e) { e.preventDefault(); }, false);"
    heading <- lift $ UI.h1 # set UI.text "The Legend of Zelda: The Minish Cap Randomizer Tracker"
    box <- lift UI.new
    lift $ getBody window #+ [element heading, element box]
    path <- chooseDropdown (askUI box) "Setting" logicFiles
    settings <- chooseDropdown (askUI box) "Setting" $ ("Default Settings", chooseDefaults) :| [("Custom Settings", askUI box)]
    logicText <- liftIO $ readFile path
    logic <- parseLogicFile settings path logicText
    case logic of
        Left err -> void $ lift $ return box #+ [UI.pre # set UI.text (show err)]
        Right (rules, info) -> do
            items <- lift $ UI.new #. "items items_global"
            locations <- lift $ UI.new #. "locations locations_global"
            locations_local <- lift $ UI.new #. "locations locations_local"
            let newItem' itemV = do
                    item <- lift $ lift $ UI.string $ itemName itemV
                    (itemChanges, itemHandler) <- liftIO newEvent
                    itemValue <- liftIO $ stepper 0 itemChanges
                    lift $ lift $ on UI.click item $ \() -> liftIO $ do
                        v <- currentValue itemValue
                        itemHandler (v + 1)
                    lift $ lift $ on UI.contextmenu item $ \(_,_) -> liftIO $ do
                        v <- currentValue itemValue
                        when (v > 0) $ itemHandler (v - 1)
                    lift $ lift $ sink UI.class_ (fmap (\i -> "item item_" ++ itemName itemV ++ bool (" region_item region_item_" ++ itemRegion itemV) "" (itemRegion itemV == "") ++ " count_" ++ show i) itemValue) $ return item
                    lift $ lift $ sink UI.text (fmap (\i -> itemName itemV ++ ": " ++ show i) itemValue) $ return item
                    lift $ lift $ return item # set UI.id_ ("item_" ++ itemName itemV ++ bool ('_' : itemRegion itemV) "" (itemRegion itemV == ""))
                    if itemRegion itemV == "" then void $ lift $ lift $ return items #+ [element item]
                    else modify $ Map.alter (Just . maybe ([],[element item]) (second (element item:))) (itemRegion itemV)
                    return $ tidings itemValue itemChanges
                newHelper' _ _ = return ()
                newLocation' loc accessible = do
                    location <- lift $ lift $ UI.string $ locationName loc
                    (locationVisitedChanges, locationVisit) <- liftIO newEvent
                    locationVisited <- liftIO $ stepper False locationVisitedChanges
                    lift $ lift $ on UI.click location $ \() -> liftIO $ locationVisit True
                    lift $ lift $ on UI.contextmenu location $ \(_,_) -> liftIO $ locationVisit False
                    lift $ lift $ flip (sink UI.class_) (return location) $ (\b b' -> "location location_" ++ locationName loc ++ bool (" region_location region_location_" ++ locationRegion loc) "" (locationRegion loc == "") ++ bool " inaccessible" " accessible" b ++ bool " not_visited" " visited" b') <$> facts accessible <*> locationVisited
                    lift $ lift $ return location # set UI.id_ ("location_" ++ locationName loc ++ bool ('_' : locationRegion loc) "" (locationRegion loc == ""))
                    if locationRegion loc == "" then void $ lift $ lift $ return locations #+ [element location]
                    else modify $ Map.alter (Just . maybe ([element location],[]) (first (element location:))) (locationRegion loc)
                    return ()
            regions <- flip execStateT Map.empty $ buildLogicNetwork rules (Callbacks newItem' newHelper' newLocation')
            unless (null regions) $ do
                (shownRegionE, showRegion) <- liftIO newEvent
                shownRegionB <- liftIO $ stepper "" shownRegionE
                selectors <- lift $ UI.new # set UI.id_ "region_selectors"
                let closeRegion = (UI.string "close" #. "close_region") >>= \c -> (on UI.click c (\() -> liftIO $ showRegion "") >> return c)
                regionsElements <- forM (Map.assocs regions) $ \(region, (regionLocations, regionItems)) -> do
                    regionSelector <- lift $ UI.string region #. "region_selector" # set UI.id_ ("region_selector_" ++ region)
                    lift $ on UI.click regionSelector $ \() -> liftIO $ showRegion region
                    lift $ element selectors #+ [element regionSelector]
                    regionItemsElement <- lift $ UI.new #. "region_items items" #+ regionItems
                    regionLocationsElement <- lift $ UI.new #. "region_locations locations" #+ regionLocations
                    regionElement <- lift $ UI.new #+ [closeRegion, element regionItemsElement, element regionLocationsElement]
                    lift $ sink UI.class_ (fmap ((("region region_" ++ region) ++) . bool " hidden_region" " shown_region" . (== region)) shownRegionB) $ element regionElement
                    return $ element regionElement
                void $ lift $ element locations_local #+ (element selectors : regionsElements)
            lift $ return box #+ [element items, element locations, element locations_local]
            return ()

askUI :: Element -> SettingsChoices (ContT () UI)
askUI parent = settingsChoices flag dropdown where
    flag name defaultValue = do
        checkbox <- lift $ UI.input # set UI.type_ "checkbox" # set UI.checked defaultValue
        label <- lift $ UI.string name
        confirm <- lift $ UI.button # set UI.text "Ok"
        el <- lift $ makeSelectionSpace #+ [element checkbox, element label, element confirm]
        lift $ return parent #+ [element el]
        shiftT $ \c -> lift $ on UI.click confirm c
        value <- lift $ checkbox # get UI.checked
        lift $ delete el
        return value
    dropdown (x :| xs) = do
        select <- lift $ UI.select #+ [UI.option # set UI.text t | (t,_) <- x:xs] # set UI.selection (Just 0)
        confirm <- lift $ UI.button # set UI.text "Ok"
        el <- lift $ makeSelectionSpace #+ [element select, element confirm]
        lift $ return parent #+ [element el]
        shiftT $ \c -> lift $ on UI.click confirm c
        selectedNum <- fmap (fromMaybe 0) $ lift $ select # get UI.selection
        lift $ delete el
        return $ snd $ (x:xs) !! selectedNum
    makeSelectionSpace = UI.new #. "settingsChoice"
