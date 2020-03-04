{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
module Tracker.Logic where

import Data.Text (Text)
import Data.Monoid
import Data.Foldable
import Control.Lens.TH

data Rule b f l = Rule {
            _ruleHead :: l,
            _ruleBody :: f b
            } deriving (Functor, Show, Eq, Ord)

data LogicExpr p = Conjunction [LogicExpr p] | Disjunction [LogicExpr p] | Count Int [(p, Int)]
    deriving (Functor, Foldable, Traversable, Eq, Ord, Show)

$(makeLenses ''Rule)
$(makePrisms ''LogicExpr)

eval :: LogicExpr Int -> Bool
eval (Conjunction conj) = all eval conj
eval (Disjunction disj) = any eval disj
eval (Count n prims) = n <= getSum (foldMap (Sum . uncurry (*)) prims)
