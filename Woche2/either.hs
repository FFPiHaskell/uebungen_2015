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


functorTest1 = P.fmap (P.+ 1) (Right 2) -- sollte Right 3 geben
functorTest2 = P.fmap (P.+ 1) (Left 2)  -- sollte Left 2 geben
applicativeTest1 = P.pure (P.*) A.<*> (Right 2)   A.<*> (Right 2) -- Sollte Right 4 geben
applicativeTest2 = P.pure (P.*) A.<*> (Left "ok") A.<*> (Right 2) -- Sollte Left "ok" geben
mFun1 = (\_ -> Right 3)
mFun2 = (Right P.. (P.+ 3))
mFun3 = (\_ -> Left "ok")
monadTest1 = do               -- sollte Left "ok" geben
                   a <- mFun1 0
                   b <- mFun2 a
                   c <- mFun3 b
                   P.return c

monadTest2 = mFun1 0 P.>>= mFun2 -- sollte Right 6 geben
