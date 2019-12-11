{-# LANGUAGE DeriveGeneric #-}
module Data.Logic where

import GHC.Generics
import Data.Aeson

data Rule = Rule { ruleHead :: Location, ruleType :: RuleType, requirements :: Logic } deriving (Eq, Ord, Show, Generic)

data RuleType = RuleLocation | RuleHelper deriving (Eq, Ord, Show, Generic)

data Location = Location { locationName :: String, locationRegion :: String } deriving (Eq, Ord, Show, Generic)

data Item = Item { itemName :: String, itemRegion :: String } deriving (Eq, Ord, Show, Generic)

data Logic = Conjunction [Logic]
           | Disjunction [Logic]
           | Count Int [(Either Item Location, Int)] deriving (Eq, Ord, Show, Generic)

foldLogic :: (a -> a -> a, a) -> (a -> a -> a, a) -> (Int -> [(Either Item Location, Int)] -> a) -> Logic -> a
foldLogic conj disj count = go where
    go (Conjunction ls) = uncurry foldl conj $ fmap go ls
    go (Disjunction ls) = uncurry foldl disj $ fmap go ls
    go (Count i xs) = count i xs

mentioned :: Rule -> [Either Item Location]
mentioned r = Right (ruleHead r) : mentionedLogic (requirements r)

mentionedLogic :: Logic -> [Either Item Location]
mentionedLogic (Conjunction ls) = ls >>= mentionedLogic
mentionedLogic (Disjunction ls) = ls >>= mentionedLogic
mentionedLogic (Count _ ls) = fmap fst ls



instance FromJSON Rule where
instance FromJSON RuleType where
instance FromJSON Location where
instance FromJSON Item where
instance FromJSON Logic where
instance ToJSON Rule where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON RuleType where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Location where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Item where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Logic where
    toEncoding = genericToEncoding defaultOptions
