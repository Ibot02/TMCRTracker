{-# LANGUAGE ViewPatterns #-}
module Main where

import System.Directory
import System.IO
import qualified Data.ByteString.Lazy as BS

import Data.Aeson

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import LogicParser
import Data.Logic
import Reactive.Logic
import Data.Items
import Data.Locations

import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe, maybe, catMaybes)
import Data.List (stripPrefix)
import Data.Bool (bool)
import Control.Monad (when, void, unless, forM, forM_)
import Control.Arrow (first, second, (&&&))

import Data.Map (Map())
import qualified Data.Map as Map
import Data.Set (Set())
import qualified Data.Set as Set

import Control.Monad.Trans.Cont
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Trans.State (execStateT, modify, StateT())

main :: IO ()
main = do
        itemsData <- loadItems
        locationsMap <- loadLocationsMap
        startGUI defaultConfig{ jsStatic = Just "./resources/"} (setup itemsData locationsMap)

logicFiles :: IO (NonEmpty (String, FilePath))
logicFiles = return $ ("default", "resources/default.logic") :| []

settingsPresets :: (Monad m) => (String, SettingsChoices m) -> IO [(String, SettingsChoices m)]
settingsPresets (fallbackName, fallback) = do
    settingsFiles <- filter ((\s -> (==s) . reverse . take (length s) . reverse) ".settings") <$> listDirectory "resources/"
    choosers <- fmap catMaybes <$> forM settingsFiles $ \file -> do
        contents <- lines <$> readFile ("resources/" ++ file)
        case contents of
            [] -> return Nothing
            (presetName : choices) -> do
                let flag settingsType flagName defaultValue =
                        case catMaybes (fmap (stripPrefix ("!flag - " ++ flagName ++ " - ")) contents) of
                            [] -> chooseFlag fallback settingsType flagName defaultValue
                            ("true":_) -> return True
                            ("false":_) -> return False
                            _ -> return defaultValue
                    dropdown settingsType dropdownName (c:|cs) =
                        case catMaybes (fmap (stripPrefix ("!dropdown - " ++ dropdownName ++ " - ")) contents) of
                            [] -> chooseDropdown fallback settingsType dropdownName (c :| cs)
                            (c':_) -> case filter ((== c') . fst) (c:cs) of
                                        [] -> return $ snd c
                                        ((_,v):_) -> return v
                return $ Just (presetName, SettingsChoices flag dropdown)
    return (choosers ++ [(fallbackName, fallback)])

setup :: ItemsData -> LocationsMap -> Window -> UI ()
setup itemsData locationsMap window = evalContT $ do
    lift $ return window # set UI.title "The Minish Cap Randomizer Tracker"
    lift $ UI.addStyleSheet window "style.css"
    lift $ UI.addStyleSheet window "items.css"
    lift $ UI.addStyleSheet window "locations.css"
    lift $ runFunction $ ffi "document.addEventListener('contextmenu', function (e) { e.preventDefault(); }, false);"
    heading <- lift $ UI.h1 # set UI.text "The Legend of Zelda: The Minish Cap Randomizer Tracker"
    box <- lift UI.new
    lift $ getBody window #+ [element heading, element box]
    path <- liftIO logicFiles >>= chooseDropdown (askUI box) "Setting" "Logic" -- todo: allow choosing any logic file instead
    settings <- (fmap (("Default Settings", chooseDefaults) :|) $ liftIO (settingsPresets ("Custom Settings", askUI box))) >>= chooseDropdown (askUI box) "Setting" "Settings"
    logicText <- liftIO $ readFile path
    logic <- parseLogicFile settings path logicText
    case logic of
        Left err -> void $ lift $ return box #+ [UI.pre # set UI.text (show err)]
        Right (rules, info) -> do
            items <- lift $ UI.new #. "items items_global"
            locations <- lift $ UI.new #. "locations locations_global map"
            locations_local <- lift $ UI.new #. "locations locations_local"
            locations_text <- lift $ UI.new #. "locations locations_global locations_text"
            let newItem' itemV = do
                    item <- lift $ lift $ UI.string (itemName itemV ++ ": ") # set UI.title__ (itemName itemV)
                    (itemChanges, itemHandler) <- liftIO newEvent
                    itemValue <- liftIO $ stepper 0 itemChanges
                    lift $ lift $ on UI.click item $ \() -> liftIO $ do
                        v <- currentValue itemValue
                        itemHandler (v + 1)
                        --todo: add max values
                    lift $ lift $ on UI.contextmenu item $ \(_,_) -> liftIO $ do
                        v <- currentValue itemValue
                        when (v > 0) $ itemHandler (v - 1)
                    let countClasses 0 = " count_0"
                        countClasses i | i > 0 = " count_" ++ show i ++ countClasses (i-1)
                        countClasses _ = ""
                        staticClasses = "item item_" ++ itemName itemV ++ bool (" region_item region_item_" ++ itemRegion itemV) "" (itemRegion itemV == "")
                    lift $ lift $ sink UI.class_ (fmap (\i -> staticClasses ++ countClasses i) itemValue) $ return item
                    lift $ lift $ return item #+ [sink UI.text (fmap show itemValue) $ UI.string "" #. "count_text"]
                    lift $ lift $ return item # set UI.id_ ("item_" ++ itemName itemV ++ bool ('_' : itemRegion itemV) "" (itemRegion itemV == ""))
                    if itemRegion itemV == "" then void $ lift $ lift $ return items #+ [element item]
                    else modify $ first $ Map.alter (Just . maybe ([],[element item]) (second (element item:))) (itemRegion itemV)
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
                    if locationRegion loc == "" then
                        void $ lift $ lift $ return locations_text #+ [element location]
                    else modify $ first $ Map.alter (Just . maybe ([element location],[]) (first (element location:))) (locationRegion loc)
                    modify $ second $ Map.insert loc (accessible, locationVisited, locationVisit)
                    return ()
            (regions, locationsData) <- flip execStateT (Map.empty, Map.empty) $ buildLogicNetwork rules (Callbacks newItem' newHelper' newLocation')
            (shownRegionE, showRegion) <- liftIO (newEvent :: IO (Event (Either (Int,Int) String), Either (Int,Int) String -> IO ()))
            shownRegionB <- liftIO $ stepper (Right "") shownRegionE
            selectors <- lift $ UI.new # set UI.id_ "region_selectors"
            let closeRegion = (UI.string "close" #. "close_region") >>= \c -> (on UI.click c (\() -> liftIO $ showRegion (Right "")) >> return c)
            regionsElements <- forM (Map.assocs regions) $ \(region, (regionLocations, regionItems)) -> do
                regionSelector <- lift $ UI.string region #. "region_selector region_selector_text" # set UI.id_ ("region_selector_text_" ++ region)
                lift $ on UI.click regionSelector $ \() -> liftIO $ showRegion (Right region)
                lift $ on UI.contextmenu regionSelector $ \(_,_) -> liftIO $ showRegion (Right "")
                lift $ element selectors #+ [element regionSelector]
                forM_ (filter ((==) (Location "" region) . pinLocation) $ locationsPositions locationsMap) $ \pinData -> do
                    pin <- lift $ UI.new # set UI.title__ region
                    let pinClasses = "location pin pin_region pin_region_" ++ region ++ " pin_pos_" ++ show (pinPositionX pinData) ++ "_" ++ show (pinPositionY pinData)
                    lift $ sink UI.class_ (pure pinClasses) $ element pin
                    lift $ on UI.click pin $ \() -> liftIO $ showRegion (Right region)
                    lift $ on UI.contextmenu pin $ \(_,_) -> liftIO $ showRegion (Right "")
                    lift $ return locations #+ [element pin]
                regionItemsElement <- lift $ UI.new #. "region_items items" #+ regionItems
                regionLocationsElement <- lift $ UI.new #. "region_locations locations" #+ regionLocations
                regionElement <- lift $ UI.new #+ [closeRegion, element regionItemsElement, element regionLocationsElement]
                lift $ sink UI.class_ (fmap ((("region region_" ++ region) ++) . bool " hidden_region" " shown_region" . (== Right region)) shownRegionB) $ element regionElement
                return $ element regionElement
            void $ lift $ element locations_local #+ (element selectors : regionsElements)
            let pinGroups = (Map.assocs $ Map.fromListWith (++) $ fmap ((pinPositionX &&& pinPositionY) &&& ((:[]) . pinLocation)) (filter ((`Map.member` locationsData) . pinLocation) $ locationsPositions locationsMap))
            forM_ pinGroups $ \((x,y),locs) -> case locs of
                [loc] -> do
                    pin <- lift $ UI.new # set UI.title__ (locationName loc)
                    let pinClasses a v = "location pin pin_pos_" ++ show x ++ "_" ++ show y ++ " pin_location pin_location_" ++ locationName loc ++ bool " inaccessible" " accessible" a ++ bool " not_visited" " visited" v
                        (locAccessible, locVisitedB, visit) = locationsData Map.! loc
                    lift $ sink UI.class_ (pinClasses <$> facts locAccessible <*> locVisitedB) $ element pin
                    lift $ on UI.click pin $ \() -> liftIO $ visit True
                    lift $ on UI.contextmenu pin $ \(_,_) -> liftIO $ visit False
                    lift $ element locations #+ [element pin]
                _ -> do
                    group <- lift UI.new
                    let groupClasses v = "region group" ++ bool " hidden_region" " shown_region" v
                    lift $ sink UI.class_ (groupClasses . (==) (Left (x,y)) <$> shownRegionB) $ element group
                    (locationsElements, status) <- fmap unzip $ forM locs $ \loc -> do
                        location <- lift $ UI.string $ locationName loc
                        let (accessible, locationVisited, locationVisit) = locationsData Map.! loc
                        lift $ on UI.click location $ \() -> liftIO $ locationVisit True
                        lift $ on UI.contextmenu location $ \(_,_) -> liftIO $ locationVisit False
                        lift $ flip (sink UI.class_) (return location) $ (\b b' -> "location location_" ++ locationName loc ++ bool (" region_location region_location_" ++ locationRegion loc) "" (locationRegion loc == "") ++ bool " inaccessible" " accessible" b ++ bool " not_visited" " visited" b') <$> facts accessible <*> locationVisited
                        return (element location, (facts accessible, locationVisited))
                    lift $ element group #+ locationsElements
                    lift $ element locations_local #+ [element group]
                    pin <- lift UI.new
                    lift $ on UI.click pin $ \() -> liftIO $ showRegion (Left (x,y))
                    lift $ on UI.contextmenu pin $ \(_,_) -> liftIO $ showRegion (Right "")
                    let accessVisitInfoB = traverse (uncurry (liftA2 (,))) status
                        pinClasses accessVisitInfo = (++) ("pin location pin_group pin_pos_" ++ show x ++ "_" ++ show y) $ case (all snd accessVisitInfo, any (\(a,v) -> a && not v) accessVisitInfo, all (uncurry (||)) accessVisitInfo) of
                            (True, _, _) -> " visited"
                            (False, _, True) -> " accessible not_visited"
                            (False, True, False) -> " part_accessible not_visited"
                            (False, False, False) -> " inaccessible not_visited"
                        pinTitle accessVisitInfo = case (length (filter (not . snd) accessVisitInfo), length (filter (uncurry (&&) . (fst &&& (not . snd))) accessVisitInfo), length accessVisitInfo) of
                                (n2,n1,n3) -> show n1 ++ "/" ++ show n2 ++ " (" ++ show n3 ++ ")"
                    lift $ sink UI.class_ (pinClasses <$> accessVisitInfoB) $ element pin
                    lift $ sink UI.title__ (pinTitle <$> accessVisitInfoB) $ element pin
                    lift $ element locations #+ [element pin]
            lift $ return box #+ [element items, element locations, element locations_local, element locations_text]
            return ()

loadItems :: (MonadIO m) => m ItemsData
loadItems = do
    (itemsData, t) <- tryReadItemsData "resources/items.json"
    b <- liftIO $ doesFileExist "resources/css/items.css"
    t' <- liftIO $ if b then Just <$> getModificationTime "resources/css/items.css" else return Nothing
    when (maybe True (<= t) t') $ createItemsCSS itemsData
    return itemsData
        where
        tryReadItemsData path = do
            b <- liftIO $ doesFileExist path
            d <- liftIO $ if b then liftA2 (liftA2 (,)) (decode <$> BS.readFile path) (Just <$> getModificationTime path) else return Nothing
            case d of
                Just (itemsData, t) -> return (itemsData, t)
                Nothing -> do
                    itemsIcons <- liftIO $ listDirectory "resources/items/"
                    let itemsData = guessItemsData itemsIcons
                    liftIO $ BS.writeFile path $ encode itemsData
                    t <- liftIO $ getModificationTime path
                    return (itemsData, t)

createItemsCSS :: (MonadIO m) => ItemsData -> m () 
createItemsCSS itemsData = do
    liftIO $ copyFile "resources/css/items.css.static" "resources/css/items.css"
    void $ liftIO $ withFile "resources/css/items.css" AppendMode $ \handle ->
        forM (concat $ items itemsData) $ \(ItemInfo (itemCssName -> name) (filename:filenames) _ showCountB) -> do
            let alwaysRule = ".item_" ++ name ++ " {\n\twidth: 32px;\n\theight: 32px;\n\ttext-indent: -9999999px;\n\tbackground: url(/static/items/" ++ filename ++ ") top left no-repeat;\n\tbackground-size: contain;\n\torder:0;\n}\n"
                surpressCount = ".item_" ++ name ++ " .count_text {\n\ttext-indent: -9999999px;\n}\n"
                furtherCountImages = concatMap (\(count, filename') -> ".item_" ++ name ++ ".count_" ++ show count ++ " {\n\tbackground: url(/static/items/" ++ filename' ++ ") top left no-repeat;\n\tbackground-size: contain;\n\torder:0;\n}\n") $ countFrom 2 filenames
                countFrom n [] = []
                countFrom n (x:xs) = (n,x) : countFrom (n+1) xs
            hPutStr handle $ alwaysRule ++ furtherCountImages ++ if showCountB then "" else surpressCount

itemCssName :: Item -> String
itemCssName (Item name region) | region == "" = helper "" $ reverse name
                               | otherwise = helper "" (reverse name) ++ "_" ++ helper "" (reverse region) where
    helper s "" = s
    helper s ('.':s') = helper ('\\':'.':s) s'
    helper s (c:s') = helper (c:s) s'

--todo: create locations.css
loadLocationsMap :: IO LocationsMap
loadLocationsMap = do
    b <- doesFileExist "resources/locations.json"
    l <- if b then (liftA2 (,)) <$> (fmap decode $ BS.readFile "resources/locations.json") <*> fmap Just (getModificationTime "resources/locations.json") else return Nothing
    case l of
        Just (locationsMap, t) -> do
            b' <- doesFileExist "resources/css/locations.css"
            createLocationsCssB <- if b' then (< t) <$> getModificationTime "resources/css/locations.css" else return True
            when createLocationsCssB $ createLocationsCss locationsMap
            return locationsMap
        Nothing -> do
            createLocationsCss defaultLocationsMap
            return defaultLocationsMap

createLocationsCss :: LocationsMap -> IO ()
createLocationsCss locationsMap = do
    copyFile "resources/css/locations.css.static" "resources/css/locations.css"
    let mapRule = ".locations_global.map {\n\tbackground-image: url(" ++ locationsMapFile locationsMap ++ ");\n\twidth: " ++ show (locationsMapWidth locationsMap) ++ "px;\n\theight: " ++ show (locationsMapHeight locationsMap) ++ "px;\n}"
        pinRule x y = ".pin_pos_" ++ show x ++ "_" ++ show y ++ " {\n\tleft: " ++ show x ++ "px;\n\ttop: " ++ show y ++ "px;\n}"
        locationRule (Location name region) = ".locations_global.locations_text .location_" ++ name ++ "{\n\tdisplay:none;\n}"
    withFile "resources/css/locations.css" AppendMode $ \h -> do
        hPutStrLn h mapRule
        forM_ (Set.fromList $ fmap (pinPositionX &&& pinPositionY) $ locationsPositions locationsMap) $ \(x,y) -> hPutStrLn h $ pinRule x y
        forM_ (fmap pinLocation $ locationsPositions locationsMap) $ \loc -> hPutStrLn h $ locationRule loc
    

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
    dropdown _ (x :| xs) = do
        select <- lift $ UI.select #+ [UI.option # set UI.text t | (t,_) <- x:xs] # set UI.selection (Just 0)
        confirm <- lift $ UI.button # set UI.text "Ok"
        el <- lift $ makeSelectionSpace #+ [element select, element confirm]
        lift $ return parent #+ [element el]
        shiftT $ \c -> lift $ on UI.click confirm c
        selectedNum <- fmap (fromMaybe 0) $ lift $ select # get UI.selection
        lift $ delete el
        return $ snd $ (x:xs) !! selectedNum
    makeSelectionSpace = UI.new #. "settingsChoice"
