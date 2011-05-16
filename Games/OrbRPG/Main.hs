module Main where

import Types
import Formatting
import Control.Monad.State


main :: IO ()
main = runRPG rpgLoop initState


rpgLoop :: RPG ()
rpgLoop = do
  gzip@(_,gs,_) <- get
  output . dBox 80 . screen $ gs
  minput <- getInputLine' "❡➤ "
  case do { input <- minput;
            lookup input builtins } of
    Just rpg -> rpg
    Nothing -> 
        case do {input <- minput;
                 gs' <- parser gs input;
                 Just $ put $ act gs' gzip} of
          Nothing -> return ()
          Just rpg -> rpg
  (_,gs',_) <- get
  maybe rpgLoop (\resp -> output $ respFormat resp) (response gs')
  rpgLoop

respFormat :: String -> String
respFormat = ("⚠ " ++)
        

builtins :: [(String, RPG ())]
builtins = [("quit",cleanup)
           ,("exit",cleanup)
           ,("undo", get >>= (\gzip@(past,_,_) ->
                      when (null past)
                      (output (respFormat "No past!") >> rpgLoop)
                      >> output (respFormat "Undo!")
                      >> put (undo gzip)
                      >> rpgLoop))
           ,("redo", get >>= (\gzip@(_,_,future) ->
                      when (null future)
                      (output (respFormat "No future!") >> rpgLoop)
                      >> output (respFormat "Redo!")
                      >> put (redo gzip)
                      >> rpgLoop))
           ,("zen",get >>= (\gzip ->
                      output (respFormat "Only the present exists.") >>
                             put (zen gzip) >> rpgLoop))
           ,("reverse time", get >>= (\gzip ->
                                      output (respFormat "Bad idea.")
                                      >> put (reverseTime gzip) >> rpgLoop))
           ]


initState :: GameState
initState = GameState{you = roughStart
                     ,enemy = Nothing
                     ,scrn = initScreen
                     ,parser = initParser
                     ,response = Nothing}

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

initScreen :: GameState -> NameDesc
initScreen = const (title, unlines $ concatMap lines 
             [" ",blurb," ",command,options])
   where title = "Welcome to the Orb RPG game."
         blurb = centerScr $
                 "This game is an RPG with orbs. You may or may not like "++
                 "the part with the orbs. I'll try to make it fun."
         command = "Select a Player:"
         options = indent . bulletList . unlines $ map name initOptions

initParser :: String -> Maybe Action
initParser = simpleparser . 
             addAction (setParser idParser)  .
             addAction (setScreen sndScreen) .
             map (\(s,pl) -> (s,setYou pl)) . nameList $ initOptions


sndScreen :: GameState -> NameDesc
sndScreen = const ("Bye",centerScr "Bye!")
sndParser :: String -> Maybe Action
sndParser _ = Nothing

cleanup :: RPG ()
cleanup = output (respFormat "Okay, bye!")