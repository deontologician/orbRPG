{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Game.OrbRPG.Types where


import System.Console.Haskeline
import Control.Monad.Trans
import Control.Monad.State
import System.Random

import Game.OrbRPG.Combine

class Describable d where
    name :: d -> String
    desc :: d -> String

type NameDesc = (String,String)

instance Describable NameDesc where
    name = fst
    desc = snd

data EnergyType = Abstract | Symbol | Physical | Spiritual | Regress | Progress
                deriving (Show,Read)
instance Describable EnergyType where
    name = show
    desc = energyDesc

type Damage = (Int,EnergyType)
dmgType :: Damage -> EnergyType
dmgType = snd

dmgAmt :: Damage -> Int
dmgAmt = fst

-- For a given orb type, gives the calculation of its damage types and points
-- towards that type
baseEnergy :: Orb -> [Damage]
baseEnergy = doitup
    where 
      go a e1 e2 = [(fromEnum a,e1),(fromEnum a,e2)]
      doitup Null = []
      doitup White = []
      doitup Black = []
      doitup (P p) = go p Abstract Physical
      doitup (L l) = go l Abstract Symbol
      doitup (E e) = go e Physical Symbol
      doitup (G g) = go g Spiritual Progress
      doitup (D d) = go d Spiritual Regress
      doitup (T t) = go t Progress Regress




-- describes the different Energy types
energyDesc :: EnergyType -> String
energyDesc Abstract = "Thought energy apart from concrete realities, " ++
                    "specific objects, or actual instances"
energyDesc Symbol = "An energy used for or regarded as representing " ++ 
                    "something often immaterial."
energyDesc Physical = "Energy pertaining to the properties of matter " ++
                      "other than those peculiar to living matter."
energyDesc Spiritual = "Energy of or pertaining to the spirit  or soul,"++
                       " as distinguished from the physical nature."
energyDesc Regress = "Energy to revert to an earlier or less advanced "++
                     "state or form."
energyDesc Progress = "Energy of movement toward a goal or to a further"++
                      " or higher stage."


data Slots = Slots {
                   }
           deriving (Show, Read)

emptySlots :: Slots
emptySlots = Slots {}

data Player = Player {playerName :: String
                     ,playerDesc :: String
                     ,hp :: Integer
                     ,weapons :: [Orb]
                     ,items :: [Orb]
                     ,orbs :: [Orb]
                     ,primSlot :: Maybe Primary
                     ,lettSlot :: Maybe Letter
                     ,godSlot  :: Maybe God
                     ,demoSlot :: Maybe Demon
                     ,elemSlot :: Maybe Element
                     ,techSlot :: Maybe Tech
                     }
            deriving (Show, Read)
instance Describable Player where
    name = playerName
    desc = playerDesc

-- The game state zipper (past,present,future)
type StateZipper = ([GameState],GameState,[GameState])
-- Next, here is the overall Monad stack
type RPG = StateT StateZipper (InputT IO)

runRPG :: RPG a -> GameState -> IO a
runRPG r s =
    let
        myInputT = runStateT r ([],s,[])
        myIO = runInputT defaultSettings myInputT
    in myIO >>= (\(a,_) -> return a)

-- Undoes one state
undo :: StateZipper -> StateZipper
undo (p:past,pres,future) = (past,p,pres:future)
undo z = z

-- Redoes one state
redo :: StateZipper -> StateZipper
redo (past,pres,f:future) = (pres:past,f,future)
redo z = z

-- Performs an action on the current state, wiping out the previous future
act :: Action -> StateZipper -> StateZipper
act a (past,pres,_) = (pres:past,a pres,[])

-- Only the present exists. Get it?
zen :: StateZipper -> StateZipper
zen (_,pres,_) = ([],pres,[])

-- Reverses time. I don't really think this is a good idea, but who knows?
reverseTime :: StateZipper -> StateZipper
reverseTime (past,pres,future) = (future,pres,past)


getInputLine' :: String -> RPG (Maybe String)
getInputLine' = lift . getInputLine

output :: String -> RPG ()
output = lift . outputStrLn

-- What do we need for Game state?
-- Two players.
-- That's it? Environmental effects? Maybe later.
-- Some kind of representation of the game
data GameState = GameState { you :: Player
                           ,enemy :: Maybe Player
                           ,scrn :: GameState -> NameDesc
    -- A list of valid inputs and the actions to perform for each of them
                           ,parser :: String -> Maybe Action
                           ,response :: Maybe String
                           ,seed :: StdGen
}

setYou :: Player -> Action
setYou pl gs = gs{you = pl}

setEnemy :: Maybe Player -> Action
setEnemy pl gs = gs{enemy = pl}

setScreen :: (GameState -> NameDesc) -> Action
setScreen scr gs = gs{scrn = scr}

setParser :: (String -> Maybe Action) -> Action
setParser f gs = gs{parser = f}

setResponse :: String -> Action
setResponse str gs = gs{response = Just str}

noResponse :: Action
noResponse gs = gs{response = Nothing}


infoResp :: String -> String
infoResp = ("⚙ " ++)

succeedResp :: String -> String 
succeedResp = ("✓ " ++ )

failResp :: String -> String
failResp = ("✗ " ++)
       

-- Basically, the way records work the function technically gets the gamestate
-- but never really gets to look at the rest of the gamestate. This remedies
-- that issue
screen :: GameState -> NameDesc
screen gs = scrn gs gs

-- What is an action? Well I'll tell you.
type Action = GameState -> GameState
-- Hey, actions are composable, awesome

-- A simple parsing strategy
simpleparser :: [(String,Action)] -> String -> Maybe Action
simpleparser cmds str = lookup str cmds

--Simplest!
noParser :: String -> Maybe Action
noParser = const Nothing

nameList :: (Describable a) => [a] -> [(String,a)]
nameList lst = zip (map name lst) lst


addAction :: Action -> [(String,Action)] -> [(String,Action)]
addAction a = map (second (a .))