module Main where

import System.Console.Haskeline
import Types
import Formatting


main :: IO ()
main = runInputT defaultSettings loop
    where loop :: InputT IO ()
          loop = do
            minput <- getInputLine "% "
            case minput of
              Nothing -> return ()
              Just "quit" -> return ()
              Just input -> do outputStrLn $ "Input was: " ++ input
                               loop

-- do
--   putStr mainScreen
--   mainLoop dummyVal

-- These validate inputs and do the work of the game basically
data Validator = Validator {
      validate :: String -> Either (IO Validator) (IO String)}


roughStart :: Player
roughStart = Player{playerName = "Roughgagh"
                   ,playerDesc = "A rough and tough fighter."
                   ,hp = 15
                   ,weapons = []
                   ,items = []
                   ,orbs = []
                   ,primSlot = Nothing
                   ,lettSlot = Nothing
                   ,godSlot = Nothing
                   ,demoSlot = Nothing
                   ,elemSlot = Nothing
                   ,techSlot = Nothing}

asalaStart :: Player
asalaStart = Player{playerName = "Asala"
                   ,playerDesc = "A tough and rough fighter."
                   ,hp = 15
                   ,weapons = []
                   ,items = []
                   ,orbs = []
                   ,primSlot = Nothing
                   ,lettSlot = Nothing
                   ,godSlot = Nothing
                   ,demoSlot = Nothing
                   ,elemSlot = Nothing
                   ,techSlot = Nothing}

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

