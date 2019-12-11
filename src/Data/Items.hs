{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Items (
    ItemsData(..),
    ItemInfo(..),
    guessItemsData
) where

import GHC.Generics
import Data.Aeson

import Data.Function ((&), on)
import Data.List (sortBy)

import Data.Logic

newtype ItemsData = ItemsData {
                items :: [[ItemInfo]]
                } deriving (Show, Eq, Ord, Generic)

data ItemInfo = ItemInfo {
                itemInfoName :: Item,
                itemInfoImages :: [String],
                itemInfoMaxCount :: Maybe Int,
                itemInfoShowCount :: Bool
                } deriving (Show, Eq, Ord, Generic)

instance FromJSON ItemInfo where
instance FromJSON ItemsData where
instance ToJSON ItemInfo where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON ItemsData where
    toEncoding = genericToEncoding defaultOptions

guessItemsData :: [String] -> ItemsData --derives ItemsData from available files
guessItemsData files = files & sortBy (flip compare `on` dropExtension)
                             & groupBy (takeWhile (not . (== '_')) . dropExtension) []
                             & map toItemInfo
                             & (:[])
                             & ItemsData      where
                        dropExtension = reverse . tail . dropWhile (not . (== '.')) . reverse
                        groupBy p xs [] = xs
                        groupBy p [] (x:xs) = groupBy p [[x]] xs
                        groupBy p ((x:xs):ys) (z:zs) | p x == p z = groupBy p ((z:x:xs):ys) zs
                                                     | otherwise  = groupBy p ([z]:(x:xs):ys) zs
                        toItemInfo [filename] | dropExtension filename `endsWith` 'X' = ItemInfo (Item (takeWhile (not . (== '_')) filename) "") [filename] Nothing True
                        toItemInfo filenames@((dropExtension -> name):_) = ItemInfo (Item name "") filenames (Just $ length filenames) False
                        endsWith (reverse -> (x:xs)) y = x == y
                        endsWith [] _ = False
