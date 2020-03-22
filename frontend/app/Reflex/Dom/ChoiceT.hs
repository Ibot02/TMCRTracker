{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DerivingVia #-}
module Reflex.Dom.ChoiceT (
    execChoiceT,
    UserChoiceQuery(..),
    UserChoice(..),
    userChoiceKey,
    hashByte
) where

import Control.Lens

import Control.Monad

import Control.Monad.Trans
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Reader (ReaderT(..))
import Control.Monad.Trans.State.Strict (StateT(..), state, evalStateT)
import Control.Monad.Reader

import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import Data.Map (Map())
import qualified Data.Map.Monoidal as MM
import Data.Map.Monoidal (MonoidalMap())
import qualified Data.Map as Map
import qualified Data.List.NonEmpty as NE
import Data.Text (Text())
import qualified Data.Text as T
import Text.Read (readMaybe)
import Data.Monoid

import Data.Default.Class

import Data.Word (Word8)

import Reflex.Dom
import Reflex.Dom.Builder.Class
import Reflex.Query.Class

import Control.Monad.Choice

newtype ChoiceT t m a = ChoiceT { runChoiceT :: forall r. StateT Int (ContT (Dynamic t r) m) a }

instance (Functor m) => Functor (ChoiceT t m) where
    fmap f (ChoiceT m) = ChoiceT (fmap f m)
instance (Functor m) => Applicative (ChoiceT t m) where
    pure a = ChoiceT $ pure a
    (ChoiceT f) <*> (ChoiceT a) = ChoiceT $ f <*> a
instance (Functor m) => Monad (ChoiceT t m) where
    (ChoiceT a) >>= f = ChoiceT $ a >>= runChoiceT . f
instance MonadTrans (ChoiceT t) where
    lift a = ChoiceT $ lift $ lift a

newtype UserChoiceQuery = UserChoiceQuery { getUserChoiceQuery :: MonoidalMap (Int, UserChoice) SelectedCount }
        deriving newtype
            (Eq, Ord, Show, Semigroup, Monoid, Group, Additive)

data UserChoice = UserChoiceOption Text (NE.NonEmpty Text)
                | UserChoiceFlag Text Text Bool
                | UserChoiceNumber Text Text
                deriving stock
                    (Eq, Ord, Show)

userChoiceKey = lens getUserChoiceKey setUserChoiceKey where
    getUserChoiceKey (UserChoiceOption k _) = k
    getUserChoiceKey (UserChoiceFlag k _ _) = k
    getUserChoiceKey (UserChoiceNumber k _) = k
    setUserChoiceKey (UserChoiceOption _ options) k = UserChoiceOption k options
    setUserChoiceKey (UserChoiceFlag _ label defaultValue) k = UserChoiceFlag k label defaultValue
    setUserChoiceKey (UserChoiceNumber _ label) k = UserChoiceNumber k label

instance Query UserChoiceQuery where
    type QueryResult UserChoiceQuery = Map Text (First Int)
    crop (UserChoiceQuery q) = Map.filterWithKey (\k _ -> k `elem` keys) where
        keys = fmap (^. _2 . userChoiceKey) $ MM.keys $ MM.filter (/= mempty) q

userOptionQuery :: Int -> Text -> NE.NonEmpty (Text, v) -> UserChoiceQuery
userOptionQuery i label opts = UserChoiceQuery $ flip MM.singleton (SelectedCount 1) $ (,) i $ UserChoiceOption label $ fmap fst opts
userFlagQuery :: Int -> Text -> Text -> Bool -> UserChoiceQuery
userFlagQuery i key label defaultValue = UserChoiceQuery $ flip MM.singleton (SelectedCount 1) $ (,) i $ UserChoiceFlag key label defaultValue
userNumberQuery :: Int -> Text -> Text -> UserChoiceQuery
userNumberQuery i key label = UserChoiceQuery $ flip MM.singleton (SelectedCount 1) $ (,) i $ UserChoiceNumber key label

getResultOption :: Text -> NE.NonEmpty (Text, v) -> Map Text (First Int) -> v
getResultOption key opts res = snd $ (opts NE.!!) $ fromMaybe 0 $ getFirst $ Map.findWithDefault mempty key res
getResultFlag :: Text -> Bool -> Map Text (First Int) -> Bool
getResultFlag key defaultValue res = fromMaybe defaultValue $ fmap (/= 0) $ getFirst $ Map.findWithDefault mempty key res
getResultNumber :: Text -> Map Text (First Int) -> Int
getResultNumber key res = fromMaybe 0 $ getFirst $ Map.findWithDefault mempty key res

hashByte :: UserChoice -> Map Text (First Int) -> Word8
hashByte (UserChoiceOption key _) vals = toEnum $ fromMaybe 0 $ getFirst $ Map.findWithDefault mempty key vals
hashByte (UserChoiceFlag key _ d) vals = toEnum $ fromEnum $ fromMaybe d $ fmap (/= 0) $ getFirst $ Map.findWithDefault mempty key vals
hashByte (UserChoiceNumber key _) vals = toEnum $ fromMaybe 0 $ getFirst $ Map.findWithDefault mempty key vals

instance (MonadHold t m, Adjustable t m, MonadQuery t UserChoiceQuery m, MonadFix m) => MonadChoice (ChoiceT t m) where
    chooseOption key options = do
        i <- ChoiceT $ state $ \i -> (i, i+1)
        r <- embedDynamic $ lift $ queryDyn $ pure $ userOptionQuery i key options
        return $ getResultOption key options r
    chooseFlag key label defaultValue = do
        i <- ChoiceT $ state $ \i -> (i, i+1)
        r <- embedDynamic $ lift $ queryDyn $ pure $ userFlagQuery i key label defaultValue
        return $ getResultFlag key defaultValue r
    chooseNumber key label = do
        i <- ChoiceT $ state $ \i -> (i, i+1)
        r <- embedDynamic $ lift $ queryDyn $ pure $ userNumberQuery i key label
        return $ getResultNumber key r

embedDynamic :: forall t r m a. (MonadHold t m, Adjustable t m) => ChoiceT t m (Dynamic t a) -> ChoiceT t m a
embedDynamic m = do
    d <- m
    initial <- lift $ sample $ current d
    ChoiceT $ lift $ shiftT $ \c -> lift $ fmap join $ widgetHold (c initial) (fmap c $ updated d)

execChoiceT :: (Reflex t, Monad m) => ChoiceT t m r -> m (Dynamic t r)
execChoiceT = evalContT . fmap constDyn . flip evalStateT 0 . runChoiceT
