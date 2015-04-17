module State where

import Control.Applicative --nötig unter ghc 7.8, Warnung bei ghc 7.10

newtype State s a = State { runState :: s -> (a,s) }

instance Functor (State s) where
  fmap f s = undefined

instance Applicative (State s) where
  pure     = undefined
  fs <*> s = undefined

instance Monad (State s) where
  return   = undefined
  s >>= fs = undefined
