{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Monad.Choice (
    MonadChoice(..),
    runWithDefaultChoices
) where

import Data.List.NonEmpty (NonEmpty(..))
import Data.Text (Text())
import Data.ByteString (ByteString())
import Data.Functor.Identity

import Control.Monad.Trans
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.Trans.State
import Control.Monad.Trans.Except

newtype DefaultChoices a = DefaultChoices { runWithDefaultChoices' :: Identity a }
                            deriving (Eq, Ord, Show, Functor, Applicative, Monad)

class Monad m => MonadChoice m where
    chooseOption :: Text -> NonEmpty (Text, opt) -> m opt
    default chooseOption :: (m ~ t m', MonadTrans t, MonadChoice m') => Text -> NonEmpty (Text, opt) -> m opt
    chooseOption key opts = lift $ chooseOption key opts
    chooseFlag :: Text -> Text -> Bool -> m Bool
    default chooseFlag :: (m ~ t m', MonadTrans t, MonadChoice m') => Text -> Text -> Bool -> m Bool
    chooseFlag key prompt d = lift $ chooseFlag key prompt d
    chooseNumber :: Text -> Text -> m Int
    default chooseNumber :: (m ~ t m', MonadTrans t, MonadChoice m') => Text -> Text -> m Int
    chooseNumber key prompt = lift $ chooseNumber key prompt

runWithDefaultChoices :: DefaultChoices a -> a
runWithDefaultChoices = runIdentity . runWithDefaultChoices'

instance MonadChoice DefaultChoices where
    chooseOption _ ((_,a):|_) = return a
    chooseFlag _ _ b = return b
    chooseNumber _ _ = return 0

instance (MonadChoice m) => MonadChoice (ReaderT r m) where
instance (Monoid w, MonadChoice m) => MonadChoice (WriterT w m) where
instance (MonadChoice m) => MonadChoice (StateT s m) where
instance (MonadChoice m) => MonadChoice (ExceptT s m) where
