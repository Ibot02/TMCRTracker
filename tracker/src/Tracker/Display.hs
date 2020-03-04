{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
module Tracker.Display (
    TrackerDisplayData(..),
    itemImages,
    itemProgressiveOverrides,
    specialLocationImages,
    scopeIcon,
    getCurrentImage,
    getNextImage
) where

import Tracker
import Reflex.Dom.Image

import Data.List.NonEmpty
import Data.Map (Map)
import qualified Data.Text as T

import Control.Lens
import Control.Applicative

data TrackerDisplayData = TrackerDisplayData {
    _itemImages :: Map ItemName Image,
    _itemProgressiveOverrides :: Map ItemName (NonEmpty Image),
    _specialLocationImages :: Map LocationName Image,
    _scopeIcon :: Image
    }

$(makeLenses 'TrackerDisplayData)

getCurrentImage :: TrackerDisplayData -> ItemName -> Int -> Maybe Image
getCurrentImage disp item 0 = disp ^? itemImages . ix item
                          <|> disp ^? itemProgressiveOverrides . ix item . ix 0
                          <|> let item' = T.takeWhile (not . (== ':')) item in if item' /= item then getCurrentImage disp item' 0 else Nothing
getCurrentImage disp item (pred -> count) = disp ^? itemProgressiveOverrides . ix item . ix count
                                        <|> disp ^? itemImages . ix item
                                        <|> let item' = T.takeWhile (not . (== ':')) item in if item' /= item then getCurrentImage disp item' (succ count) else Nothing

getNextImage :: TrackerDisplayData -> ItemName -> Int -> Maybe Image
getNextImage disp item n = disp ^? itemImages . ix item
                       <|> disp ^? itemProgressiveOverrides . ix item . ix n
                       <|> let item' = T.takeWhile (not . (== ':')) item in if item' /= item then getNextImage disp item n else Nothing
