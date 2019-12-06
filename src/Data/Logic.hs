module Data.Logic where

data Rule = Rule { ruleHead :: Location, ruleType :: RuleType, requirements :: Logic } deriving (Eq, Ord, Show)

data RuleType = RuleLocation | RuleHelper deriving (Eq, Ord, Show)

data Location = Location { locationName :: String, locationRegion :: String } deriving (Eq, Ord, Show)

data Item = Item { itemName :: String, itemRegion :: String } deriving (Eq, Ord, Show)

data Logic = Conjunction [Logic]
           | Disjunction [Logic]
           | Count Int [(Either Item Location, Int)] deriving (Eq, Ord, Show)

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
