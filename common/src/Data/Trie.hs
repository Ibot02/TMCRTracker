{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Trie (
    Trie(..),
    trieNode,
    trieChildren
)where

import Control.Lens
import Data.Map (Map())
import qualified Data.Map as Map

data Trie k a = Trie {
    _trieNode :: a,
    _trieChildren :: Map k (Trie k a)
    } deriving stock (Functor, Foldable, Traversable)

$(makeLenses 'Trie)

type instance Index (Trie k a) = [k]
type instance IxValue (Trie k a) = a

instance (Ord k) => Ixed (Trie k a) where
    ix [] = trieNode
    ix (k:ks) = trieChildren . ix k . ix ks

instance (Ord k, Semigroup a) => Semigroup (Trie k a) where
    Trie x c <> Trie x' c' = Trie (x <> x') (Map.unionWith (<>) c c')

instance (Ord k, Monoid a) => Monoid (Trie k a) where
    mempty = Trie mempty mempty

instance (Ord k) => FoldableWithIndex [k] (Trie k) where
instance (Ord k) => FunctorWithIndex [k] (Trie k) where
instance (Ord k) => TraversableWithIndex [k] (Trie k) where
    itraverse f (Trie node children) = Trie <$> f [] node <*> itraverse (\i t -> itraverse (f . (i:)) t) children
