module Main where

{-
 This is a litte Toy-game where you should increase
 or decrease an internal Counter by pressing u (up)
 or d (down) until you quit (q).

 Only one Function needs to be implemented and the
 steps are laid out there.

 Tasks:
 - Write the Input-Function getInput
 - Write the Main-Loop.
-}

import Control.Monad
import System.IO
import Control.Monad.Trans.RWS
import Control.Monad.IO.Class
import Data.Functor

data Input = Up
           | Down
           | Quit
           | Invalid
           deriving (Show, Eq)

data Env = Env
           { upKey   :: Char
           , downKey :: Char
           , quitKey :: Char
           }

env :: Env
env = Env 'u' 'd' 'q'

data State = State 
           { counter :: Int
           }

initialState :: State
initialState = State 0

getInput :: RWST Env () State IO Input
getInput = do
            undefined

getInputfromEnv :: Char -> Env -> Input
getInputfromEnv c e
                | c ==  upKey e   =  Up
                | c ==  downKey e =  Down
                | c ==  quitKey e =  Quit
                | otherwise       =  Invalid

main :: IO ()
main = do
        hSetBuffering stdin NoBuffering
        hSetEcho stdin False
        score <- fst <$> execRWST mainLoop env initialState
        putStrLn $ "Your Score was: " ++ show (counter score)

mainLoop :: RWST Env () State IO ()
mainLoop = do
        undefined
        --1. get Input
        --2. Increase State by 1 if Up is pressed, decrease by 1 if Down is pressed
        --   Catch ALL cases
        --3. get the new state
        --4. Print Line "<Action>: <Current counter value>"
        --5. Loop until Quit is pressed
