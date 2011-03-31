module Main where

import System.Console.Readline
import Data.List
import qualified Data.Text as T
import Combine

main :: IO ()
main = do
  putStr mainScreen
  mainLoop dummyVal

-- These validate inputs and do the work of the game basically
data Validator = Validator {
      validate :: String -> Either (IO Validator) (IO String)}

mainLoop :: Validator -> IO ()
mainLoop v = do
  inLine <- readline "RPG⊨⊛ "
  case inLine of
    Nothing -> putStrLn "Bye!"
    Just "quit" -> putStrLn "Bye!"
    Just someline -> do
                addHistory someline
                case validate v someline of
                  Right errStr -> do
                       str <- errStr
                       putStrLn str
                       mainLoop v
                  Left good -> do
                       v' <- good
                       mainLoop v'

startGame :: Validator
startGame = Validator start
    where 
      start :: String -> Either (IO Validator) (IO String)
      start s = undefined

dummyVal :: Validator
dummyVal = Validator dummy
    where
      dummy :: String -> Either (IO Validator) (IO String)
      dummy s = if s `elem` ["1","2"] 
                then Left $ do
                  if s == "1" 
                  then putStrLn $ boxCWrap "Roughgagh Selected."
                  else putStrLn $ boxCWrap "Asala Selected."
                  putStrLn mainScreen
                  return dummyVal
                else Right $ return 
                         "No, you can only select 1 or 2. Dumbass"

{- Formatting Functions -}

-- Welcome screen! (boxed)
mainScreen :: String
mainScreen = boxer $
             ["    Welcome to the RPG game.",
             "",
             "       You are probably going to like   ",
             "       this. Please check it out. OK?   ",
             "                                        ",
             "      Select a Player:                  ",
             "        1. Roughgagh                    ",
             "        2. Asala                        "]

-- Creates a line of just ═ characters
line :: Int -> String
line n = replicate n '═'

-- Centers and text wraps a string and then puts a box around it
boxCWrap :: String -> String
boxCWrap = boxer . map (\r -> center 74 r) . wrap 74

-- Just wraps the string and puts a box around it
boxWrap :: String -> String
boxWrap = boxer . wrap 74

-- puts a box around a list of strings
boxer :: [String] -> String
boxer s = topper ++ emptyline ++ makemiddle ++ emptyline ++ bottomer
    where
      fixlen str = if length str <= 74 
                   then str ++ (replicate (74 - length str) ' ')
                   else take 74 str
      emptyline = "║" ++ (replicate 78 ' ') ++ "║\n"
      makemiddle = unlines $ map (\r -> "║  " ++ fixlen r ++ "  ║") s
      topper = "╔" ++ line 78 ++ "╗\n"
      bottomer = "╚" ++ line 78 ++ "╝\n"

-- wraps text to the given line length (nonexhaustive match isn't a problem
-- because of the base value given to the fold)
wrap :: Int -> String -> [String]
wrap w = reverse . foldl' (\(h:hs) next -> if length h + length next + 1> w
                              then next:h:hs
                              else (h++" "++next):hs) [""] . words

-- centers text in a field of length w (identity if string is too long)
center :: Int -> String -> String
center w "\n" = (replicate w ' ')
center w "" = (replicate w ' ')
center w  s = T.unpack . T.init . T.unlines .
              map (T.center w ' ') . T.lines $ T.pack s
