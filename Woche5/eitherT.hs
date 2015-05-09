module Main where

{-
 In this Exercise you should implement EitherT.
 Start by defining fmap, pure and <*>. Then try
 the Monad-Instance.

 Remember: You don't have to modify data - only
 unpack and repack.

 A little Test-Program in inside the main-function
 and should work if you have done everything
 correctly.

 Output should be the following:
 10: 10
 Fehlertest == Left "Fehlertest"
-}

import Data.Functor
import Control.Applicative
import Control.Monad.IO.Class
import Control.Monad.Trans.Class (lift,MonadTrans)
import Control.Monad (liftM)


data EitherT l m r = EitherT { runEitherT :: m (Either l r) }

instance Functor f => Functor (EitherT l f) where
    fmap f = undefined

instance Applicative f => Applicative (EitherT l f) where
    pure = undefined
    ef <*> ex = undefined

instance Monad m => Monad (EitherT l m) where
    return = undefined
    x >>= f = undefined

instance MonadIO m => MonadIO (EitherT e m) where
  liftIO = undefined

instance MonadTrans (EitherT e) where
  lift = undefined

main :: IO ()
main = do
       ergebnis <- runEitherT testEither
       putStrLn $ "Fehlertest == " ++ show ergebnis

testEither :: EitherT String IO Int
testEither = do
         a <- get10
         liftIO . putStrLn $ "10: " ++ show a
         errorTest

get10 :: EitherT String IO Int
get10 = return 10

errorTest :: EitherT String IO Int
errorTest = EitherT . return . Left $ "Fehlertest"
