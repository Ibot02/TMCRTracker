module Reactive.Logic where

import Data.Logic

import Data.Map (Map())
import qualified Data.Map as Map
import Data.Set (Set())
import qualified Data.Set as Set

import Data.Graph
import Data.Array ((!))

import Control.Arrow ((&&&), first, second)
import Control.Monad (when)
import Data.Traversable (for)
import Data.Bool (bool)
import Data.Maybe (catMaybes, fromMaybe, isNothing)
import Data.Either

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad.Trans (MonadIO(), lift, liftIO)

import Reactive.Threepenny

data Callbacks m = Callbacks { newItem :: Item -> m (Tidings Int), newHelper :: Location -> Tidings Bool -> m (), newLocation :: Location -> Tidings Bool -> m () }


buildLogicNetwork :: (MonadIO m) => [Rule] -> Callbacks m -> m ()
buildLogicNetwork rules c = do
    let rulesMap = Map.fromList $ fmap (ruleHead &&& id) rules
        locationsAndHelpers = Map.keysSet rulesMap
        helpersAndItems = Set.fromList $ concatMap (mentionedLogic . requirements) $ Map.elems rulesMap
        items = Set.fromList $ lefts $ Set.toList $ Set.takeWhileAntitone isLeft helpersAndItems
        (dependencyGraph, getRule, findRule) = graphFromEdges $ fmap (\(loc, rule) -> (rule, locationName loc, catMaybes (fmap (either (const Nothing) (Just . locationName)) (mentionedLogic . requirements $ rule)))) $ Map.assocs rulesMap
    itemsT <- fmap Map.fromList $ for (Set.toList items) $ \i -> (,) i <$> newItem c i
    flip runStateT (itemsT, Map.empty :: Map String (Tidings Bool)) $ for (reverse (topSort dependencyGraph)) $ \k -> do
        let (rule, locName, _) = getRule k
            loc = ruleHead rule
            register = case ruleType rule of
                RuleHelper -> newHelper
                RuleLocation -> newLocation
            conj = (liftA2 (liftA2 (&&)), pure (pure True))
            disj = (liftA2 (liftA2 (||)), pure (pure False))
            count i things = fmap (fromMaybe (pure True)) $ runMaybeT $ fmap (fmap (>= i) . foldl (liftA2 (+)) (pure 0)) $ for things $ \(t, mult) ->
                case t of
                    Right loc' -> MaybeT $ do
                        v <- gets $ fmap (fmap (bool 0 mult)) . Map.lookup (locationName loc') . snd
                        when (isNothing v) $ liftIO $ putStrLn $ "failed to find helper/location " ++ show loc'
                        return v
                    Left item -> MaybeT $ do
                        v <- gets $ fmap (fmap (* mult)) . Map.lookup item . fst
                        when (isNothing v) $ liftIO $ putStrLn $ "failed to find item " ++ show item
                        return v
        tiding <- foldLogic conj disj count $ requirements rule
        lift $ register c loc tiding
        modify $ second $ Map.insert locName tiding
    return ()
