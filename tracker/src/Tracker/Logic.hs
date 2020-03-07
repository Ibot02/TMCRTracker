{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
module Tracker.Logic where

import Data.Text (Text)
import Data.Monoid
import Data.Maybe
import Data.Foldable
import Data.List (sortOn)
import Control.Lens
import Data.Functor.Foldable
import Data.Functor.Foldable.TH

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Map (Map())
import qualified Data.Map as Map
import Data.Set (Set())
import qualified Data.Set as Set

import Data.Heap (Heap(), FstMinPolicy(), MinPrioHeap())
import qualified Data.Heap as Heap

data Rule b f l = Rule {
            _ruleHead :: l,
            _ruleBody :: f b
            } deriving (Functor, Show, Eq, Ord)

data LogicExpr p = Conjunction [LogicExpr p] | Disjunction [LogicExpr p] | Count Int [(p, Int)]
    deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

$(makeLenses ''Rule)
$(makePrisms ''LogicExpr)
$(makeBaseFunctor ''LogicExpr)

eval :: LogicExpr Int -> Bool
eval = cata f where
    f (ConjunctionF conj) = and conj
    f (DisjunctionF disj) = or disj
    f (CountF n prims) = n <= getSum (foldMap (Sum . uncurry (*)) prims) 

substitute :: (i -> Either i' Int) -> LogicExpr i -> LogicExpr i'
substitute s = hoist (f s) where
    f s (CountF n prims) = helper s n prims
    f _ (ConjunctionF c) = ConjunctionF c
    f _ (DisjunctionF d) = DisjunctionF d
    helper s n [] = CountF n []
    helper s n ((p,c):ps) = let CountF n' ps' = helper s n ps in case s p of
        Left p' -> CountF n' ((p',c):ps')
        Right 0 -> CountF n' ps'
        Right v -> CountF (n' - v * c) ps'

substituteMany :: (p -> [p']) -> LogicExpr p -> LogicExpr p'
substituteMany s = hoist (f s) where
    f s (CountF n prims) = CountF n [(p',c) | (p, c) <- prims, p' <- s p]
    f _ (ConjunctionF c) = ConjunctionF c
    f _ (DisjunctionF d) = DisjunctionF d

data SphereCount = Sphere Int | UnknownSphere
                 deriving (Eq, Ord, Show)

incrementSphere :: SphereCount -> SphereCount
incrementSphere (Sphere i) = Sphere (succ i)
incrementSphere UnknownSphere = UnknownSphere

evalSphereCount :: LogicExpr SphereCount -> SphereCount
evalSphereCount = cata f where
    f (ConjunctionF c) = maximum (Sphere 0 : c)
    f (DisjunctionF d) = minimum (UnknownSphere : d)
    f (CountF n ps) = countTo n $ (Sphere 0, 0) : sortOn fst ps
    countTo :: Int -> [(SphereCount, Int)] -> SphereCount
    countTo !n ((Sphere i, c):ps) | n <= c = Sphere i
                                  | otherwise = countTo (n-c) ps
    countTo _ _ = UnknownSphere

data SphereState i l = SphereState {
                    sphereStateLogic :: Map l (LogicExpr (Either i l)),
                    sphereStateRelevants :: Map (Either i l) (Set l),
                    sphereStateItems :: Map l i,
                    sphereStateSpheres :: Map l SphereCount
                    } deriving (Eq, Ord, Show)

initialSphereState :: (Ord i, Ord l) => [(i, Int)] -> [Rule (Either i l) LogicExpr l] -> SphereState i l
initialSphereState untrackedItems rules = SphereState logic relevants mempty spheres where
        logic = Map.fromListWith (\r1 r2 -> Disjunction [r1,r2]) $ [(_ruleHead r, replaceUntracked (_ruleBody r)) | r <- rules]
        relevants = Map.fromListWith (<>) $ [(i, Set.singleton l) | (l, exp) <- Map.toList logic, i <- toList exp]
        spheres = updateSpheres (SphereState logic relevants mempty (fmap (const UnknownSphere) logic)) (Heap.fromList $ [(UnknownSphere, l)| l <- Map.keys logic]) 
        replaceUntracked = substitute replaceUntracked'
        untrackedItems' = Map.fromList untrackedItems
        replaceUntracked' (Right l) = Left (Right l)
        replaceUntracked' (Left i) = maybe (Left (Left i)) Right $ Map.lookup i untrackedItems'

updateSphereState :: (Ord i, Ord l) => [(l, Maybe i)] -> SphereState i l -> SphereState i l
updateSphereState changes state@(SphereState logic relevants items spheres) = state { sphereStateItems = items', sphereStateSpheres = spheres' } where
    (toUpdate, items') = foldl (\(toUpdate, items) (l, i') -> let n = sphereStateSpheres state Map.! l; h i = changesFromItems relevants [(n,i)]; f i = (toUpdate <> maybe mempty h i <> maybe mempty h i', i') in Map.alterF f l items) (mempty, items) changes
    spheres' = updateSpheres (state { sphereStateItems = items' }) toUpdate
        
changesFromItems :: (Ord i, Ord l) => Map (Either i l) (Set l) -> [(SphereCount, i)] -> MinPrioHeap SphereCount l
changesFromItems relevants items = Heap.fromList $ [(n, l) | (n,i) <- items, l <- toList (relevants Map.! (Left i))]

updateSpheres :: forall i l. (Ord i, Ord l) => SphereState i l -> MinPrioHeap SphereCount l -> Map l SphereCount
updateSpheres state heap = case Heap.view heap of
        Nothing -> sphereStateSpheres state
        Just ((c,l),heap') -> let n = sphereStateSpheres state Map.! l;
                                      getSphere (Left i) = fmap (incrementSphere . getLocSphere) $ Map.keys $ Map.filter (==i) $ sphereStateItems state;
                                      getSphere (Right l) = [getLocSphere l];
                                      getLocSphere l = sphereStateSpheres state Map.! l;
                                      n' = evalSphereCount $ substituteMany getSphere $ sphereStateLogic state Map.! l;
                                      heap'' :: MinPrioHeap SphereCount l;
                                      heap'' = (Heap.fromList [(min n n', l') | l' <- toList $ Map.findWithDefault mempty (Right l) $ sphereStateRelevants state]) <> (maybe mempty (\i -> changesFromItems (sphereStateRelevants state) [(min n n', i)]) $ Map.lookup l $ sphereStateItems state)
                              in if n < c || n == n' then updateSpheres state heap' else updateSpheres (state { sphereStateSpheres = Map.insert l n' (sphereStateSpheres state) }) (heap' <> heap'')

getSpheres :: SphereState i l -> Map l SphereCount
getSpheres = sphereStateSpheres
