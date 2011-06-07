module Main where

import Game.OrbRPG.Types
import Game.OrbRPG.Formatting
import Control.Monad.State
import System.Random


main :: IO ()
main = do
  sg <- getStdGen
  runRPG rpgLoop $ initState sg


rpgLoop :: RPG ()
rpgLoop = do
  gzip@(_,gs,_) <- get
  -- Output the response from the last action if there is one
  maybe (return ()) (\resp -> output resp) (response gs)
  output . dBox 80 . screen $ gs
  minput <- getInputLine' "❡➤ "
  case do { input <- minput;
            lookup input $ builtins} of
    Just (b,rpg) -> rpg >> when b rpgLoop
    Nothing -> 
        case do {input <- minput;
                 gs' <- parser gs input;
                 Just $ put $ act gs' gzip} of
          Nothing -> rpgLoop
          Just rpg -> rpg >> rpgLoop
 

-- Built in commands. The extra boolean parameter determines if the program
-- needs to loop after the action is carried out. (i.e. not exit)
builtins :: [(String, (Bool, RPG ()))]
builtins = [("quit",(False,cleanup))
           ,("exit",(False,cleanup))
           ,("undo",(True, get >>= (\gzip@(past,_,_) ->
                                    when (null past)
                                    (output . failResp $ "No past!")
                                    >> output "↶ Undo!"
                                    >> put (undo gzip))))
           ,("redo",(True, get >>= (\gzip@(_,_,future) ->
                                    when (null future)
                                    (output (failResp "No future!"))
                                    >> output "↷ Redo!"
                                    >> put (redo gzip))))
           ,("zen", (True, get >>= (\gzip ->
                                    output "☯ Only the present exists." >>
                                    put (zen gzip))))
           ,("reverse time",(True, get >>= (\gzip ->
                                            output "☢ You've made a huge mistake."
                                            >> put (reverseTime gzip))))
           ]


initState :: StdGen -> GameState
initState sg = GameState{you = roughStart
                     ,enemy = Nothing
                     ,scrn = welcomeScreen
                     ,parser = initParser
                     ,response = Nothing
                     ,seed = sg}

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

welcomeScreen :: GameState -> NameDesc
welcomeScreen = const (title, unlines $ concatMap lines 
             [blurb," ",command,options])
   where title = "Welcome to the Orb RPG game."
         blurb = centerScr $
                 "This game is an RPG with orbs. You may or may not like "++
                 "the part with the orbs. I'll try to make it fun."
         command = "Select a Player:"
         options = boxList 20 initOptions

initParser :: String -> Maybe Action
initParser str = 
    case simpleparser parselist str of
      Nothing -> Just (setResponse . failResp $ "No character by that name")
      Just action -> Just action
    where 
      parselist = addAction (setParser gameOverParser) .
                  addAction (setScreen gameOverScr) .
                            map (\(s,pl) -> (s,setYou pl . resp pl)) 
                                    . nameList $ initOptions
      resp pl = setResponse . succeedResp $ "You chose " ++ name pl ++ "."


gameOverScr :: GameState -> NameDesc
gameOverScr = const ("Sorry.",centerScr "You are dead, the game has ended. ☹")
gameOverParser :: String -> Maybe Action
gameOverParser _ = Nothing

cleanup :: RPG ()
cleanup = output (infoResp "Okay, bye!")