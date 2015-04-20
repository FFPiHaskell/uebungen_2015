module Identity where

import Control.Applicative --nötig für ghc 7.8, Warnung bei 7.10

newtype Identity a = Identity { runIdentity :: a }

instance Functor Identity where
  fmap f i = undefined

instance Applicative Identity where
  pure     = undefined
  fi <*> i = undefined

instance Monad Identity where
  return    = undefined
  i >>= fi  = undefined
