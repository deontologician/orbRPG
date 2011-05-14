module Main where

import Types
import Formatting
import Control.Monad.State


main :: IO ()
main = runRPG rpgLoop initState


rpgLoop :: RPG ()
rpgLoop = do
  gzip@(past,gs,future) <- get
  output . boxer . screen $ gs
  minput <- getInputLine' "❡➤ "
  case minput of
    Nothing -> rpgLoop
    Just "quit" -> cleanup
    Just "exit" -> cleanup
    Just "undo" -> when (null past) 
                     (output "Can't undo!" >> rpgLoop)
                   >> output "undo!" >> put (undo gzip) >> rpgLoop
    Just "redo" -> when (null future) 
                      (output "Can't redo!" >> rpgLoop)
                   >> output "redo!" >> put (redo gzip) >> rpgLoop
    Just "zen"  -> output "zen!" >> put (zen gzip) >> rpgLoop
    Just "reverse time" -> output "Bad idea." >> put (reverseTime gzip) >> rpgLoop
    Just input -> put (act gs' gzip) >> rpgLoop
        where gs' = parser gs input
        


initState :: GameState
initState = GameState{you = undefined
                     ,enemy = undefined
                     ,scrn = initScreen
                     ,parser = initParser}

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

initOptions :: [Player]
initOptions = [roughStart, asalaStart]

initScreen :: GameState -> String
initScreen = const . unlines $ concatMap lines 
             [title," ",blurb," ",command,options]
   where title = centerScr "Welcome to the Orb RPG game."
         blurb = cWrapScr $ 
                 "This game is an RPG with orbs. You may or may not like "++
                 "the part with the orbs. I'll try to make it fun."
         command = "Select a Player:"
         options = indent . bulletList . unlines $ map name initOptions

initParser :: String -> Action
initParser = simpleparser . 
             addAction (setParser idParser)  .
             addAction (setScreen sndScreen) .
             map (\(s,pl) -> (s,setYou pl)) . nameList $ initOptions


sndScreen :: GameState -> String
sndScreen = const "Bye!"
sndParser :: String -> Action
sndParser _ = id

cleanup :: RPG ()
cleanup = output "Okay, bye!"