module Either where

import qualified Prelude as P
import qualified Control.Applicative as A

data Either a b = Left a
                | Right b
                deriving (P.Show, P.Eq)

instance P.Functor (Either a) where
  fmap f e = P.undefined

instance A.Applicative (Either a) where
  pure      = P.undefined
  ef <*> ev = P.undefined

instance P.Monad (Either a) where
  return    = P.undefined
  v >>= f   = P.undefined
