module Main where

import Types
import Formatting
import Control.Monad.State


main :: IO ()
main = runRPG rpgLoop initState


rpgLoop :: RPG ()
rpgLoop = do
  gzip@(_,gs,_) <- get
  output . boxer . screen $ gs
  minput <- getInputLine' "❡➤ "
  case do { input <- minput;
            lookup input builtins } of
    Just rpg -> rpg
    Nothing -> 
        case do {input <- minput;
                 gs' <- parser gs input;
                 Just $ put $ act gs' gzip} of
          Nothing -> rpgLoop
          Just rpg -> rpg
        

builtins :: [(String, RPG ())]
builtins = [("quit",cleanup)
           ,("exit",cleanup)
           ,("undo", get >>= (\gzip@(past,_,_) ->
                      when (null past)
                      (output (boxer "No past!") >> rpgLoop)
                      >> output "Undo!" 
                      >> put (undo gzip)
                      >> rpgLoop))
           ,("redo", get >>= (\gzip@(_,_,future) ->
                      when (null future)
                      (output "No future!" >> rpgLoop)
                      >> output "Redo!" 
                      >> put (redo gzip)
                      >> rpgLoop))
           ,("zen",get >>= (\gzip ->
                      output "Zen! Only the present exists." >>
                             put (zen gzip) >> rpgLoop))
           ,("reverse time", get >>= (\gzip ->
                      output "Bad idea." >> put (reverseTime gzip) >> rpgLoop))
           ]


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

initParser :: String -> Maybe Action
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