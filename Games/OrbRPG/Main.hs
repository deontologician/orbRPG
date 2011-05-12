module Main where

import Types
import Formatting
import Control.Monad.State


main :: IO ()
main = runRPG rpgLoop initState



rpgLoop :: RPG ()
rpgLoop = do
  cs <- get
  printOut initRound cs
  minput <- getInputLine' "++ "
  case minput of
    Nothing -> return ()
    Just "exit" -> return ()
    Just input -> do outputStrLn' $ "Input was: " ++ input
                     rpgLoop


initState :: GameState
initState = GameState{you = roughStart
                     ,enemy = asalaStart}


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


initRound :: Round
initRound = Round{ printOut = const . outputStrLn' $ mainScreen }

-- Welcome screen! (boxed)
mainScreen :: String
mainScreen = boxer $
             ["    Welcome to the Orb RPG game.",
             "",
             "       This game is an RPG with orbs    ",
             "       You may or may not like the part ",
             "       with the orbs.                   ",
             "                                        ",
             "      Select a Player:                  ",
             "        1. Roughgagh                    ",
             "        2. Asala                        "]

