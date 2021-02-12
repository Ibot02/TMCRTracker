{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Tracker.TMCR.Tracker (
    getTracker,
    getTrackerDisplayData
) where


import Data.FileEmbed

import Data.Map (Map())
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T
import Data.Text (Text(), pack)
import Data.Foldable (toList)
import Control.Lens
import qualified Data.ByteString as BS
import Data.Text.Encoding

import Tracker
import Tracker.Logic
import Tracker.Display

import Tracker.TMCR.Logic

import Data.Trie
import Reflex.Dom.Image
import Control.Monad.Choice

import Control.Monad.IO.Class
import Reflex.Dom ((=:))

getTracker :: (MonadChoice m) => m Tracker
getTracker = getTrackerFor defaultLogic

getTrackerFor :: (MonadChoice m) => LogicFile -> m Tracker
getTrackerFor logicFile = do
    logic' <- chooseSettings logicFile
    let (logic, dungeonLocations) = case logic' of
            Right logic -> (fmap toRule $ snd logic, getDungeonLocs $ snd logic)
            Left errorMsg -> error (T.unpack errorMsg)
        extras = mempty
            & untrackedItems .~ [("Rupee200", 5)]
            & nonLogicTrackedItems .~ [] 
            & extrasComboItems .~ ("Books" =: ComboData ["HyruleanBestiary", "PicoriLegend", "MaskHistory"] SimpleCombo) 
            & itemAliases .~ [(item <> dungeonSubtype, item <> dungeonName ) | item <- ["SmallKey", "BigKey"], (dungeonSubtype, dungeonName) <- [(".0x18",":Deepwood"),(".0x19",":FlameCave"),(".0x1A",":Fortress"),(".0x1B",":Droplets"),(".0x1C",":Palace"),(".0x1D",":DHC"),(".0x1E",":Crypt")]]
            & itemMaxAmounts .~ Trie (Map.fromList [("SmithSword", 5), ("Boomerang", 2), ("SmallKey.0x18", 4), ("Shield", 2), ("BombBag", 4), ("SmallKey.0x19", 2), ("Bow", 2), ("SmallKey.0x1A", 4), ("Bottle4", 4), ("Wallet", 3), ("SmallKey.0x1B", 4), ("HeartContainer", 17), ("SmallKey.0x1C", 6), ("KinstoneX.YellowTotemProng", 3), ("KinstoneX.YellowTornadoProng", 5), ("PieceOfHeart", 44), ("SmallKey.0x1E", 3), ("SmallKey.0x1D", 5)]) mempty
        prizes = ["EarthElement", "FireElement", "WindElement", "WaterElement"] --todo make dynamic based on actual prizes
        itemWidget = TrackerWidgetGrid [
                        [SimpleItem "SmithSword", SimpleItem "GustJar", SimpleItem "PacciCane", SimpleItem "Boomerang", SimpleItem "Flippers", SimpleItem "SpinAttack", SimpleItem "SwordBeam", SpecialLocation "DeepwoodPrize" prizes, SimpleItem "SmallKey.0x18", SimpleItem "BigKey.0x18", SimpleItem "EarthElement"],
                        [SimpleItem "Shield", SimpleItem "MoleMitts", SimpleItem "LanternOff", SimpleItem "BombBag", SimpleItem "GripRing", SimpleItem "RockBreaker", SimpleItem "PerilBeam", SpecialLocation "CoFPrize" prizes, SimpleItem "SmallKey.0x19", SimpleItem "BigKey.0x19", SimpleItem "FireElement"],
                        [SimpleItem "PegasusBoots", SimpleItem "RocsCape", SimpleItem "Ocarina", SimpleItem "Bow", SimpleItem "PowerBracelets", SimpleItem "DashAttack", SimpleItem "RollAttack", SpecialLocation "FortressPrize" prizes, SimpleItem "SmallKey.0x1A", SimpleItem "BigKey.0x1A", SimpleItem "WindElement"],
                        [SimpleItem "Bottle4", SimpleItem "DogFoodBottle", SimpleItem "WakeUpMushroom", SimpleItem "JabberNut", SimpleItem "Wallet", SimpleItem "DownThrust", SimpleItem "FastSplit", SpecialLocation "DropletsPrize" prizes, SimpleItem "SmallKey.0x1B", SimpleItem "BigKey.0x1B", SimpleItem "WaterElement"],
                        [ComboItem "Books", SimpleItem "GraveyardKey", SimpleItem "LonLonKey", SimpleItem "TingleTrophy", SimpleItem "HeartContainer", SimpleItem "GreatSpin", SimpleItem "LongSpin", SpecialLocation "PalacePrize" prizes, SimpleItem "SmallKey.0x1C", SimpleItem "BigKey.0x1C"],
                        [SimpleItem "KinstoneX.YellowTotemProng", SimpleItem "KinstoneX.YellowCrown", SimpleItem "KinstoneX.YellowTornadoProng", SimpleItem "CarlovMedal", SimpleItem "PieceOfHeart", SimpleItem "FastSpin", SimpleItem "SmallKey.0x1E", SpecialLocation "KingGift" prizes, SimpleItem "SmallKey.0x1D", SimpleItem "BigKey.0x1D"]]
        intf = TrackerInterface [itemWidget] (maps prizes dungeonLocations)
    return $ restrictToRelevantLocations $ Tracker intf extras logic

getTrackerDisplayData :: (Applicative m) => m TrackerDisplayData
getTrackerDisplayData = pure $(do
                            let imagesFromDir = fmap (\(path, contents) -> (reverse . tail . dropWhile (/= '.') . reverse $ path, Image path contents))
                            itemDir <- fmap imagesFromDir $ liftIO $ getDir "resources/items"
                            locationDir <- fmap imagesFromDir $ liftIO $ getDir "resources/locations"
                            [|let splitNum name image = case dropWhile (/= '_') name of
                                      "" -> Left image
                                      (_:num) -> Right (read num :: Int, image);
                                  itemsFromDir = fmap (fmap (fmap snd . NE.sortWith fst) . Map.mapKeysWith (<>) (takeWhile (/= '_')) . fmap (:| [])) . Map.mapEitherWithKey splitNum . Map.fromList;
                                  (items, progressiveItems) = itemsFromDir itemDir;
                                  locations = Map.fromList locationDir
                                  scopeIconImage = items Map.! "Map"
                               in TrackerDisplayData (Map.mapKeysMonotonic T.pack items) (Map.mapKeysMonotonic T.pack progressiveItems) (Map.mapKeysMonotonic T.pack locations) scopeIconImage|])



defaultLogic :: LogicFile
defaultLogic = case preParseLogic $ decodeUtf8 $(embedFile "resources/default.logic") of
        Right logicFile -> logicFile
        Left _ -> error "something went wrong while parsing the logic file"

-- embedLogicFile :: Q Expr
-- embedLogicFile fileName = do
--         contents <- liftIO $ BS.readFile fileName
--         case preParseLogic $ decodeUtf8 contents of
--             Left e -> error $ "Error reading logic file: " <> fileName <> "\n" <> T.unpack e
--             Right _ -> return ()
--         bsToExp contents

maps :: [ItemName] -> Map Text [LocationName] -> Trie Text TrackerWidgets
maps prizes dungeonLocations = Trie overworldMap (
        "MelariMines" =: Trie [melariMinesMap] mempty
     <> "Dungeons" =: dungeonMaps prizes dungeonLocations)

overworldMap :: TrackerWidgets
overworldMap = return $ TrackerWidgetMap $ LocationMapNested $$(staticImage "resources/maps/OverworldThumbnail.png") 164 131 $ Map.fromList $
        [ ("Mt. Crenel", (0,0,40,48, mtCrenelMap))
        , ("Base of Mt. Crenel", (0, 49, 40, 16, mtCrenelBaseMap))
        , ("Castor Wilds", (0, 66, 40, 40, castorMap))
        , ("Wind Ruins", (0, 107, 40, 24, ruinsMap))
        , ("Royal Valley", (41, 0, 20, 48, valleyMap))
        , ("Trilby Highlands", (41, 49, 20, 40, trilbyMap))
        , ("Western Wood", (41, 90, 20, 40, westMap))
        , ("Hyrule Castle Garden", (62, 0, 40, 32, gardenMap))
        , ("North Hyrule Field", (62, 33, 40, 28, northMap))
        , ("Hyrule Town", (62, 62, 40, 40, townMap))
        , ("South Hyrule Field", (62, 103, 40, 28, southMap))
        , ("Veil Falls", (103, 0, 20, 48, fallsMap))
        , ("Lon Lon Ranch", (103, 50, 28, 40, llrMap))
        , ("Eastern Hills", (103, 91, 20, 40, eastMap))
        , ("Cloud Tops", (124, 0, 40, 40, cloudsMap))
        , ("Lake Hylia", (132, 41, 32, 48, lakeMap))
        , ("Minish Woods", (124, 91, 40, 40, woodsMap))
        ]

mtCrenelMap :: LocationMapArea
mtCrenelMap = LocationMapArea $$(staticImage "resources/maps/overworld/MtCrenel.png") 0 4 176 208 $ Map.fromList $
    [ ((84, 60), LocationMapPinLocations [Left ["Dungeons", "FlameCave"]])
    , ((24, 8), LocationMapPinLocations [Right ("CrenelBeanstalk" <> x) | x <- "HP" : ["Rupee" <> pack (show n) | n <- [1..8]]])
    , ((124, 24), LocationMapPinLocations [Right "CrenelRainPath"])
    , ((8, 56), LocationMapPinLocations [Right "CrenelDigCaveHeartPiece"])
    , ((136, 80), LocationMapPinLocations [Right "CrenelBlockChest"])
    , ((112, 88), LocationMapPinLocations [Left ["MelariMines"], Right "Melari", Right "PreMelariFusion"])
    , ((36, 108), LocationMapPinLocations [Right "CrenelClimbFusion"])
    , ((56, 128), LocationMapPinLocations [Right "CrenelBombFairy"])
    , ((156, 96), LocationMapPinLocations [Right ("Grayblade" <> x) | x <- ["Scroll", "HeartPiece", "Left", "Right"]])
    , ((152, 120), LocationMapPinLocations [Right "CrenelGripScrub"])
    , ((28, 156), LocationMapPinLocations [Right "CrenelFairyHeartPiece"])
    , ((148, 164), LocationMapPinLocations [Right "CrenelCaveDownstairs"])
    , ((8, 192), LocationMapPinLocations [Right ("CrenelHeartCave" <> x) | x <- ["HeartPiece", "Left", "Right"]])
    ]
mtCrenelBaseMap :: LocationMapArea
mtCrenelBaseMap = LocationMapArea $$(staticImage "resources/maps/overworld/MtCrenelBase.png") 0 214 176 88 $ Map.fromList $
    [ ((20, 20), LocationMapPinLocations [Right "CrenelLowerLeftFusion"])
    , ((64, 16), LocationMapPinLocations [Right "CrenelMinishHouse"])
    , ((80, 8), LocationMapPinLocations [Right "CrenelVineHole"])
    , ((116, 8), LocationMapPinLocations [Right ("CrenelFairyCaveRupee" <> pack (show n)) | n <- [1..3]])
    , ((140, 60), LocationMapPinLocations [Right "CrenelOutsideRupee"])
    , ((152, 40), LocationMapPinLocations [Right "CrenelGreenWaterFusion"])
    ]
castorMap :: LocationMapArea
castorMap = LocationMapArea $$(staticImage "resources/maps/overworld/CastorWilds.png") 0 312 176 168 $ Map.fromList $
    [ ((8, 16), LocationMapPinLocations [Right ("Wilds" <> x) | x <- ["MulldozerHole", "TopLeftCrack"]])
    , ((36, 12), LocationMapPinLocations [Right "WildsTopRightCrack"])
    , ((8, 56), LocationMapPinLocations [Right "WildsMiddleLeftCrack"])
    , ((60, 128), LocationMapPinLocations [Right "WildsLongJourneyCrack"])
    , ((164, 132), LocationMapPinLocations [Right ("WildsMinishWaterCave" <> x) | x <- ["Chest", "HP"]])
    , ((116, 8), LocationMapPinLocations [Right "WildsDekuCaveRight"])
    , ((164, 8), LocationMapPinLocations [Right x | x <- ["ScarbladeScroll", "WildsTopRightCaveHeartPiece"]])
    , ((40, 80), LocationMapPinLocations [Right "WildsTopChest"])
    , ((76, 76), LocationMapPinLocations [Right "WildsDarknutCave"])
    , ((72, 92), LocationMapPinLocations [Right "WildsDigButterfly"])
    , ((136, 72), LocationMapPinLocations [Right ("WildsDiggingCave" <> x) | x <- ["Left", "Right"]])
    , ((128, 160), LocationMapPinLocations [Right "WildsSouthCave"])
    , ((8, 128), LocationMapPinLocations [Right ("SwiftbladeTheFirst" <> x) | x <- ["Scroll", "HeartPiece"]])
    , ((172, 116), LocationMapPinLocations [Right ("WildsSunkenKinstone" <> x) | x <- ["Bottom", "Middle", "Top"]])
    ]
ruinsMap :: LocationMapArea
ruinsMap = LocationMapArea $$(staticImage "resources/maps/overworld/WindRuins.png") 0 482 176 96 $ Map.fromList $
    [ ((156, 8), LocationMapPinLocations [Left ["Dungeons", "Fortress"]])
    , ((8, 24), LocationMapPinLocations [Right "RuinsArrowButterfly"])
    , ((32, 8), LocationMapPinLocations [Right "RuinsBombCave"])
    , ((12, 68), LocationMapPinLocations [Right "RuinsMinishHome"])
    , ((68, 80), LocationMapPinLocations [Right "RuinsPillarsFusion"])
    , ((52, 64), LocationMapPinLocations [Right "RuinsMinishCaveHeartPiece"])
    , ((76, 52), LocationMapPinLocations [Right "RuinsCrackFusion"])
    , ((56, 16), LocationMapPinLocations [Right "RuinsBeanStalk"])
    , ((144, 52), LocationMapPinLocations [Right ("RuinsArmosKill" <> x) | x <- ["Left", "Right"]])
    , ((168, 32), LocationMapPinLocations [Right "RuinsBombBag"])
    ]
valleyMap :: LocationMapArea
valleyMap = LocationMapArea $$(staticImage "resources/maps/overworld/RoyalValley.png") 178 36 88 176 $ Map.fromList $
    [ ((40, 16), LocationMapPinLocations [Left ["Dungeons", "Crypt"]])
    , ((16, 24), LocationMapPinLocations [Right ("RoyalValley" <> x) | x <- ["LeftFusion", "GraveHeartPiece"]])
    , ((72, 24), LocationMapPinLocations [Right "RoyalValleyRightFusion", Right "GinaGrave"])
    , ((8, 48), LocationMapPinLocations [Right "ValleySwimButterfly"])
    , ((72, 68), LocationMapPinLocations [Right "DampeKey"])
    , ((20, 128), LocationMapPinLocations [Right "RoyalValleyLostWoodsChest"])
    , ((72, 120), LocationMapPinLocations [Right "ArrowFairy"])
    ]
trilbyMap :: LocationMapArea
trilbyMap = LocationMapArea $$(staticImage "resources/maps/overworld/TrilbyHighlands.png") 178 218 88 168 $ Map.fromList $
    [ ((64, 16), LocationMapPinLocations [Right "TrilbyTopFusion"])
    , ((16, 40), LocationMapPinLocations [Right ("TrilbyMoleCave" <> x) | x <- ["Left", "Right"]])
    , ((48, 40), LocationMapPinLocations [Right "TrilbyWaterCave"])
    , ((48, 80), LocationMapPinLocations [Right "TrilbyMiddleFusion"])
    , ((24, 88), LocationMapPinLocations [Right "TrilbyBombCave"])
    , ((48, 112), LocationMapPinLocations [Right "BottleScrub"])
    , ((8, 120), LocationMapPinLocations [Right ("TrilbyPuddleCaveRupee" <> pack (show n)) | n <- [1..15]])
    ]
westMap :: LocationMapArea
westMap = LocationMapArea $$(staticImage "resources/maps/overworld/WesternWood.png") 178 388 88 176 $ Map.fromList $
    [ ((32, 8), LocationMapPinLocations [Right ("WesternWoodTopDig" <> pack (show n)) | n <- [1..6]])
    , ((40, 40), LocationMapPinLocations [Right "WesternWoodChest"])
    , ((28, 84), LocationMapPinLocations [Right "WesternTreeHouseHP"])
    , ((72, 88), LocationMapPinLocations [Right ("WesternWoodBottomDig" <> pack (show n)) | n <- [1..2]])
    , ((24, 120), LocationMapPinLocations [Right ("PercyHouse" <> x) | x <- ["Moblin", "Reward"]])
    , ((8, 152), LocationMapPinLocations (Right "WesternBeanstalk" : [Right ("WesternWoodBeanstalkRupee" <> pack (show n)) | n <- [1..16]]))
    ]
gardenMap :: LocationMapArea
gardenMap = LocationMapArea $$(staticImage "resources/maps/overworld/HyruleCastleGarden.png") 268 30 176 96 $ Map.fromList $
    [ ((88, 8), LocationMapPinLocations (Left ["Dungeons", "DHC"]: [Right ("PedItem" <> pack (show n)) | n <- [1..3]]))
    , ((40, 12), LocationMapPinLocations [Right "CastleGardenLeftMinishHole"])
    , ((136, 12), LocationMapPinLocations [Right ("CastleGarden" <> x) | x <- ["RightMinishHole", "DrainedFountainHP"]])
    , ((164, 64), LocationMapPinLocations [Right ("Grimblade" <> x) | x <- ["HeartPiece", "Scroll"]])
    , ((8, 88), LocationMapPinLocations [Right "CastleWaterLeft"])
    , ((24, 88), LocationMapPinLocations [Right "CastleWaterRight"])
    ]
northMap :: LocationMapArea
northMap = LocationMapArea $$(staticImage "resources/maps/overworld/NorthHyruleField.png") 268 128 176 144 $ Map.fromList $
    [ ((16, 16), LocationMapPinLocations [Right "PreValleyFusion"])
    , ((64, 80), LocationMapPinLocations [Right "NorthHyruleFieldBuriedTreasure"])
    , ((48, 40), LocationMapPinLocations [Right "PreCastleCaveHeartPiece"])
    , ((36, 80), LocationMapPinLocations [Right "GreatbladeScroll"])
    -- , ((72, 64), LocationMapPinLocations [Right "TingleTreeBottomLeft"])
    -- , ((104, 64), LocationMapPinLocations [Right "TingleTreeBottomRight"])
    -- , ((72, 48), LocationMapPinLocations [Right "TingleTreeTopLeft"])
    -- , ((104, 48), LocationMapPinLocations [Right "TingleTreeTopRight"])
    -- , ((88, 56), LocationMapPinLocations [Right "TingleBoomerang"])
    , ((88, 56), LocationMapPinLocations (Right "TingleBoomerang":[Right ("TingleTree" <> y <> x) | x <- ["Left","Right"], y <- ["Top", "Bottom"]]))
    ]
townMap :: LocationMapArea
townMap = LocationMapArea $$(staticImage "resources/maps/overworld/HyruleTown.png") 268 274 176 168 $ Map.fromList $
    [ ((88, 48), LocationMapPinLocations [Right "TownBell"])
    , ((56, 8), LocationMapPinLocations [Right "LibraryMinish"])
    , ((32, 8), LocationMapPinLocations [Right ("FlipsCave" <> x) | x <- ["Small", "Big", "WaterfallRupee"]])
    , ((124, 16), LocationMapPinLocations [Right "SchoolTop"])
    , ((160, 16), LocationMapPinLocations (Right "SchoolMinishPathFusion" : [Right ("SchoolGarden" <> x) | x <- ["Left", "Middle", "Right", "HeartPiece"]]))
    , ((124, 32), LocationMapPinLocations [Right "HyruleWellTop"])
    , ((144, 52), LocationMapPinLocations [Right ("HyruleWell" <> x) | x <- ["Right", "Top", "Pillar"]])
    , ((128, 68), LocationMapPinLocations ([Right ("TownDigging" <> x) | x <- ["Left", "Right", "Top"]]
                                        <> [Right ("HyruleWell" <> x) | x <- ["Left", "Top", "Pillar"]]))
    , ((132, 112), LocationMapPinLocations [Right ("HyruleWell" <> x) | x <- ["Bottom", "Top", "Pillar"]])
    , ((44, 68), LocationMapPinLocations [Right "TownWaterfallChest"])
    , ((8, 96), LocationMapPinLocations [Right "WrightAtticBook"])
    , ((12, 152), LocationMapPinLocations [Right "CarlovReward"])
    , ((60, 68), LocationMapPinLocations [Right ("Fountain" <> x) | x <- ["HeartPiece", "Small", "Big"]])
    , ((60, 84), LocationMapPinLocations [Right "RemShoeShop"])
    , ((60, 100), LocationMapPinLocations [Right "SimulationChest"])
    , ((60, 116), LocationMapPinLocations [Right ("FigurineHouse" <> x) | x <- ["Left", "Middle", "Right", "HeartPiece"]])
    , ((60, 132), LocationMapPinLocations [Right "CafeLady"])
    , ((56, 156), LocationMapPinLocations [Right ("SwiftbladeScroll" <> pack (show n)) | n <- [1..4]])
    , ((116, 84), LocationMapPinLocations [Right "BakeryAttic"])
    , ((160, 64), LocationMapPinLocations [Right "HearthBackdoor"])
    , ((152, 80), LocationMapPinLocations [Right "HearthPot"])
    , ((152, 96), LocationMapPinLocations [Right "HearthLedge"])
    , ((148, 128), LocationMapPinLocations [Right "JullietaBook"])
    , ((148, 156), LocationMapPinLocations [Right "CuccoMinigame"])
    , ((104, 124), LocationMapPinLocations (Right "StockWellAttic" : [Right ("Shop" <> x <> "Item") | x <- ["80", "300", "600", "Dogfood"]]))
    ]
southMap :: LocationMapArea
southMap = LocationMapArea $$(staticImage "resources/maps/overworld/SouthHyruleField.png") 268 444 176 120 $ Map.fromList $
    [ ((16, 48), LocationMapPinLocations [Right ("LinkHousePuddleCaveRupee" <> pack (show n)) | n <- [1..15]])
    , ((64, 40), LocationMapPinLocations [Right "LinkMinishWaterHoleHeartPiece"])
    , ((136, 64), LocationMapPinLocations [Right "LinkHouseFusion"])
    , ((160, 96), LocationMapPinLocations [Right "LinkHouseTreeHP"])
    , ((164, 44), LocationMapPinLocations [Right "TingleTrophyItem"])
    , ((116, 64), LocationMapPinLocations [Right "SmithHouse", Right "IntroItem1", Right "IntroItem2"])
    , ((104, 48), LocationMapPinLocations windTribeTowerLocations)
    ]
fallsMap :: LocationMapArea
fallsMap = LocationMapArea $$(staticImage "resources/maps/overworld/VeilFalls.png") 446 0 88 216 $ Map.fromList $
    [ ((12, 40), LocationMapPinLocations [Right "FallsWaterfallHP"])
    , ((48, 48), LocationMapPinLocations (Right "FallsTopCaveBomb" : Right "FallsTopCaveFree" : [Right ("FallsUpperCave" <> d <> "Rupee" <> pack (show n)) | (d,ns) <- [("", [1..9]), ("Water", [1..6])], n <- ns]))
    , ((60, 68), LocationMapPinLocations [Right "FallsBuriedTopTreasure", Right "FallsNearCrestFusion"])
    , ((36, 96), LocationMapPinLocations [Right "FallsCliff"])
    , ((36, 112), LocationMapPinLocations [Right "FallsBehindWall"])
    , ((28, 140), LocationMapPinLocations [Right "FallsUpperHeartPiece"])
    , ((44, 144), LocationMapPinLocations [Right "FallsWaterCaveEnd", Right "FallsWaterDigCaveHP"])
    , ((72, 144), LocationMapPinLocations [Right ("FallsLower" <> x) | x <- ["Rupee" <> pack (show n) | n <- [1..3]] <> ["Cave" <> d | d <- ["Left", "Right"]]])
    , ((56, 168), LocationMapPinLocations [Right "FallsBuriedBottomTreasure"])
    , ((80, 208), LocationMapPinLocations [Right "FallsLowerHeartPiece"])
    , ((44, 184), LocationMapPinLocations [Right "SplitbladeScroll"])
    ]
cloudsMap :: LocationMapArea
cloudsMap = LocationMapArea $$(staticImage "resources/maps/overworld/CloudTops.png") 536 0 354 354 $ Map.fromList $
    mappend [
      ((84, 36), LocationMapPinLocations [Left ["Dungeons", "Palace"]])
    , ((84, 64), LocationMapPinLocations windTribeTowerLocations)
    ] $ (each . _1 . _1 +~ 178) $
    mappend [
      ((8, 8), LocationMapPinLocations [Right "TopLeftCloudWall", Right "CloudsWestLeft"])
    , ((32, 8), LocationMapPinLocations [Right "CloudsWestRight"])
    , ((16, 32), LocationMapPinLocations [Right "CloudsWestBottom"])
    , ((168, 8), LocationMapPinLocations [Right "TopRightCloudWall"])
    , ((168, 80), LocationMapPinLocations [Right "CloudsFreeChest"])
    , ((24, 152), LocationMapPinLocations [Right "CloudsSouthLeft"])
    , ((8, 160), LocationMapPinLocations [Right "BottomLeftCloudWall"])
    , ((56, 120), LocationMapPinLocations [Right "CloudsSouthMiddle"])
    , ((72, 112), LocationMapPinLocations [Right "BottomMiddleCloudWall"])
    , ((96, 120), LocationMapPinLocations [Right "CloudsSouthRight"])
    , ((120, 144), LocationMapPinLocations [Right "BottomRightCloudWall"])
    , ((168, 120), LocationMapPinLocations [Right "BottomRightmostTopCloudWall"])
    , ((160, 152), LocationMapPinLocations [Right "BottomRightmostBottomCloudWall"])
    ] $ (each . _1 . _2 +~ 178) $
    [ ((88, 8), LocationMapPinLocations [Right "CloudsNorthKill"])
    , ((96, 120), LocationMapPinLocations [Right "CloudsSouthKill"])
    ]
llrMap :: LocationMapArea
llrMap = LocationMapArea $$(staticImage "resources/maps/overworld/LonLonRanch.png") 446 218 128 168 $ Map.fromList $
    [ ((32, 8), LocationMapPinLocations [Right "LonLonFallsFusion"])
    , ((56, 32), LocationMapPinLocations [Right "LonLonBuriedTreasure"])
    , ((96, 8), LocationMapPinLocations [Right "AboveHPHole"])
    , ((96, 24), LocationMapPinLocations [Right "HyliaPostCapeCaveHeartPiece"])
    , ((84, 60), LocationMapPinLocations [Right "LonLonHeartPiece", Right "LonLonMinishPathFusion"])
    , ((44, 76), LocationMapPinLocations [Right "LonLonCave", Right "LonLonCaveSecret"])
    , ((60, 108), LocationMapPinLocations [Right "LonLonPot"])
    , ((88, 88), LocationMapPinLocations [Right "LonLonWallet"])
    , ((24, 152), LocationMapPinLocations [Right "GoronBottle4", Right "GoronSmallChest"])
    ]
lakeMap :: LocationMapArea
lakeMap = LocationMapArea $$(staticImage "resources/maps/overworld/LakeHylia.png") 576 186 136 200 $ Map.fromList $
    [ ((56, 100), LocationMapPinLocations [Left ["Dungeons", "Droplets"]])
    , ((40, 40), LocationMapPinLocations [Right "HyliaSunkenHeartPiece"])
    , ((56, 40), LocationMapPinLocations [Right "StockwellDog"])
    , ((96, 48), LocationMapPinLocations ([Right ("HyliaBeanstalk" <> x) | x <- ["Left", "Right", "HP"]]
                                       <> [Right ("HyliaCapeCave" <> x) | x <- ("Entrance" : ((<>) <$> ["Top", "Bottom"] <*> ["Left", "Middle", "Right"]))]
                                       <> [Right "HyliaPostCapeCaveHeartPiece"]))
    , ((124, 48), LocationMapPinLocations [Right "HyliaNorthMinishHole"])
    , ((96, 72), LocationMapPinLocations [Right "HyliaPreCapeCaveHeartPiece"])
    , ((88, 104), LocationMapPinLocations [Right "LibrariCabinContainer"])
    , ((88, 120), LocationMapPinLocations [Right "HyliaWaterDigCave"])
    , ((48, 152), LocationMapPinLocations [Right ("Waveblade" <> x) | x <- ["Scroll", "HeartPiece"]])
    , ((76, 168), LocationMapPinLocations [Right "HyliaBottomHeartPiece"])
    , ((120, 176), LocationMapPinLocations [Right "HyliaMinishPathFusion", Right "HyliaMayorCabin"])
    ]
eastMap :: LocationMapArea
eastMap = LocationMapArea $$(staticImage "resources/maps/overworld/EasternHills.png") 446 388 88 176 $ Map.fromList $
    [ ((8, 32), LocationMapPinLocations [Right "FarmCaveRupee"])
    , ((40, 72), LocationMapPinLocations [Right "LonLonBottle3"])
    , ((16, 104), LocationMapPinLocations [Right ("HillsBeanstalk" <> x) | x <- ["HP", "Left", "Right"]])
    , ((28, 124), LocationMapPinLocations [Right "HillsKeeseCave"])
    ]
woodsMap :: LocationMapArea
woodsMap = LocationMapArea $$(staticImage "resources/maps/overworld/MinishWoods.png") 536 388 176 176 $ Map.fromList $
    [ ((80, 104), LocationMapPinLocations [Left ["Dungeons", "Deepwood"]])
    , ((20, 8), LocationMapPinLocations [Right "MinishRupeeFairy"])
    , ((20, 36), LocationMapPinLocations [Right "MinishTopLeftFusion"])
    , ((120, 8), LocationMapPinLocations [Right "MinishWitchHut"])
    , ((88, 40), LocationMapPinLocations [Right "WitchDiggingCave"])
    , ((160, 8), LocationMapPinLocations [Right "MinishNorthHole"])
    , ((36, 60), LocationMapPinLocations [Right "MinishMiddleLeftFusion", Right "MinishHeartPieceTop"])
    , ((104, 56), LocationMapPinLocations [Right ("MinishLikeLikeDiggingCave" <> x) | x <- ["Left", "Right"]])
    , ((152, 104), LocationMapPinLocations [Right "MinishMiddleRightFusion"])
    , ((120, 124), LocationMapPinLocations [Right "MinishMiddleFusion"])
    , ((72, 120), LocationMapPinLocations [Right "MinishHeartPieceBottom"])
    , ((12, 120), LocationMapPinLocations [Right "MinishBottomLeftFusion"])
    , ((16, 96), LocationMapPinLocations [Right ("Minish" <> d <> "FlipperHole" <> x) | (d, xs) <- [("Left", ["","HeartPiece"]), ("Right", [""]), ("Middle", [""])], x <- xs])
    , ((56, 144), LocationMapPinLocations [Right "BelariBombs", Right "BelariRemote"])
    , ((80, 152), LocationMapPinLocations [Right "MinishVillageHeartPiece", Right "JabberNut", Right "MinishMinishPathFusion"])
    , ((96, 168), LocationMapPinLocations [Right "MinishCrackChest"])
    ]


windTribeTowerLocations :: [Either Scope LocationName]
windTribeTowerLocations = fmap Right $
    [ "Tower" <> x | x <- ["F2"] <> [d <> "Bed" | d <- ["Left", "Middle", "Right"]] <> [d <> d' | d <- ["Bottom", "Top"], d' <- ["Left", "Right"]]]
 <> [ "Gregal" <> x | x <- ["One", "Two"]]

melariMinesMap :: TrackerWidget
melariMinesMap = TrackerWidgetMap $ LocationMap $ LocationMapArea $$(staticImage "resources/maps/MelariMines.png") 0 0 720 608 $ Map.fromList [
                  ((424, 312), LocationMapPinLocations [Right "Melari"])
                , ((328, 328), LocationMapPinLocations [Right "MelariMining1"])
                , ((168, 488), LocationMapPinLocations [Right "MelariMining2"])
                , ((616, 520), LocationMapPinLocations [Right "MelariMining3"])
                , ((656, 120), LocationMapPinLocations [Right "MelariMining4"])
                , ((216, 72 ), LocationMapPinLocations [Right "MelariMining5"])
                , ((504, 72 ), LocationMapPinLocations [Right "MelariMining6"])
                , ((232, 232), LocationMapPinLocations [Right "MelariMining7"])
                , ((408, 504), LocationMapPinLocations [Right "MelariMining8"])
                , ((120, 56 ), LocationMapPinLocations [Right "PreMelariFusion"])
                ]

dungeonMaps :: [ItemName] -> Map Text [LocationName] -> Trie Text TrackerWidgets
dungeonMaps prizes dungeonLocations = Trie (dungeonOverview prizes) $ fmap (\m -> Trie m mempty) (
        "Deepwood"  =: deepwoodScope prizes (Map.findWithDefault [] "Deepwood" dungeonLocations)
     <> "FlameCave" =: cofScope prizes (Map.findWithDefault [] "FlameCave" dungeonLocations)
     <> "Fortress"  =: fortressScope prizes (Map.findWithDefault [] "Fortress" dungeonLocations)
     <> "Droplets"  =: todScope prizes (Map.findWithDefault [] "Droplets" dungeonLocations)
     <> "Crypt"     =: cryptScope prizes (Map.findWithDefault [] "Crypt" dungeonLocations)
     <> "Palace"    =: powScope prizes (Map.findWithDefault [] "Palace" dungeonLocations)
     <> "DHC"       =: dhcScope prizes (Map.findWithDefault [] "DHC" dungeonLocations)
        )

dungeons :: [Text]
dungeons = ["Deepwood", "FlameCave", "Fortress", "Droplets", "Crypt", "Palace", "DHC"]

dungeonOverview :: [ItemName] -> TrackerWidgets
dungeonOverview prizes = [TrackerWidgetGrid [
      fmap SimpleItem prizes
    , [SpecialLocation p prizes | p <- ["DeepwoodPrize", "CoFPrize", "FortressPrize", "DropletsPrize", "KingGift", "PalacePrize"]] <> [SpecialLocation "BeatVaati" []]
    , [SimpleItem ("SmallKey:" <> d) | d <- dungeons]
    , [SimpleItem ("BigKey:" <> d) | d <- dungeons]
    , [Scope ["Dungeons", d] | d <- dungeons]
    ]]
    
deepwoodScope :: [ItemName] -> [LocationName] -> TrackerWidgets
deepwoodScope prizes locs = 
    [ TrackerWidgetGrid [[SpecialLocation "DeepwoodPrize" prizes, SimpleItem "SmallKey:Deepwood", SimpleItem "BigKey:Deepwood"]]
    , TrackerWidgetLocationList $ fmap Right locs]
cofScope :: [ItemName] -> [LocationName] -> TrackerWidgets
cofScope prizes locs =
    [ TrackerWidgetGrid [[SpecialLocation "CoFPrize" prizes, SimpleItem "SmallKey:FlameCave", SimpleItem "BigKey:FlameCave"]]
    , TrackerWidgetLocationList $ fmap Right locs]
fortressScope :: [ItemName] -> [LocationName] -> TrackerWidgets
fortressScope prizes locs =
    [ TrackerWidgetGrid [[SpecialLocation "FortressPrize" prizes, SimpleItem "SmallKey:Fortress", SimpleItem "BigKey:Fortress"]]
    , TrackerWidgetLocationList $ fmap Right locs]
todScope :: [ItemName] -> [LocationName] -> TrackerWidgets
todScope prizes locs =
    [ TrackerWidgetGrid [[SpecialLocation "DropletsPrize" prizes, SimpleItem "SmallKey:Droplets", SimpleItem "BigKey:Droplets"]]
    , TrackerWidgetLocationList $ fmap Right locs]
cryptScope :: [ItemName] -> [LocationName] -> TrackerWidgets
cryptScope prizes locs =
    [ TrackerWidgetGrid [[SpecialLocation "KingGift" prizes, SimpleItem "SmallKey:Crypt"]]
    , TrackerWidgetLocationList $ fmap Right locs]
powScope :: [ItemName] -> [LocationName] -> TrackerWidgets
powScope prizes locs =
    [ TrackerWidgetGrid [[SpecialLocation "PalacePrize" prizes, SimpleItem "SmallKey:Palace", SimpleItem "BigKey:Palace"]]
    , TrackerWidgetLocationList $ fmap Right locs]
dhcScope :: [ItemName] -> [LocationName] -> TrackerWidgets
dhcScope prizes locs = 
    [ TrackerWidgetGrid [[SpecialLocation "BeatVaati" [], SimpleItem "SmallKey:DHC", SimpleItem "BigKey:DHC"]]
    , TrackerWidgetLocationList $ fmap Right locs]
