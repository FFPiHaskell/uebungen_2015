{-# LANGUAGE OverloadedStrings #-}

{-
 This is the parser we had in the lecture.
 Your task is to extend it to parse another log-format
 and write a function to unify both parsers into one.
 
 Tasks:
 1. write all functions which are undefined.
 2. extend the logParser-function to also accept
    the american format.
 3. Add the optional fields to the lineParser.
 4. Make the log read in a commandline argument.
-}

import Data.Time
import Data.Word
import Data.Attoparsec.Text
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as B (readFile)
import Control.Applicative

data Date = Date
            { day :: Day
            , tod :: TimeOfDay
            } deriving (Show, Eq)


data IP = IP Word8 Word8 Word8 Word8 deriving (Show, Eq)

data Device = Mouse 
            | Keyboard 
            | Monitor 
            | Speakers 
            deriving (Show,Eq)

data LogLine = LogLine Date IP Device deriving Show
-- Use this one for task 3:
-- data LogLine = LogLine Date IP Device (Maybe Source) deriving Show


type Log = [LogLine]

data Source = Internet | Friend | NoAnswer deriving Show

dateParser :: Parser Date
dateParser = do
  y  <- count 4 digit
  char '-'
  mm <- count 2 digit
  char '-'
  d  <- count 2 digit
  char ' '
  h  <- count 2 digit
  char ':'
  m  <- count 2 digit
  char ':'
  s  <- count 2 digit
  return $
    Date { day = fromGregorian (read y) (read mm) (read d)
         , tod = TimeOfDay (read h) (read m) (read s)
         }

americanDateParser :: Parser Date
americanDateParser = do
  undefined

parseIP :: Parser IP
parseIP = do
  d1 <- decimal
  char '.'
  d2 <- decimal
  char '.'
  d3 <- decimal
  char '.'
  d4 <- decimal
  return $ IP d1 d2 d3 d4

deviceParser :: Parser Device
deviceParser =
     (string "mouse"    >> return Mouse)
 <|> (string "keyboard" >> return Keyboard)
 <|> (string "monitor"  >> return Monitor)
 <|> (string "speakers" >> return Speakers)

americanDeviceParser :: Parser Device
americanDeviceParser = do
  undefined

parseSource :: Parser Source
parseSource = undefined

lineParser :: Parser LogLine
lineParser = do
     datum <- dateParser
     char ' '
     ip <- parseIP
     char ' '
     geraet <- deviceParser
     return $ LogLine datum ip geraet

americanLineParser :: Parser LogLine
americanLineParser = do
     undefined

logParser :: Parser Log
logParser = many $ lineParser <* endOfLine

main :: IO ()
main = do
     log <- B.readFile "log.txt"
     print $ parseOnly logParser (decodeUtf8 log)
