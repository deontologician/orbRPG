{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections, FlexibleInstances,
    TypeSynonymInstances
 #-}

module Types where


import System.Console.Haskeline
import Control.Monad.Trans
import Control.Monad.State

import Combine

class Describable d where
    name :: d -> String
    desc :: d -> String

type NameDesc = (String,String)

instance Describable NameDesc where
    name = fst
    desc = snd

data Weapon = Weapon {weaponName :: String
                     ,weaponDesc :: String
                     ,dmg :: (Integer,Integer)
                     ,dmgType :: EnergyType
                     }
            deriving (Show, Read)
instance Describable Weapon where
    name = weaponName
    desc = weaponDesc

data EnergyType = Abstract | Symbol | Physical | Spiritual | Regress | Progress
                deriving (Show,Read)
instance Describable EnergyType where
    name = show
    desc = energyDesc

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

data Item = HealingItm {itemName :: String
                       ,itemDesc :: String
                       ,healAmt :: (Integer, Integer)
                       }
          | DamageItm { itemName :: String
                      ,dmgAmt :: (Integer,Integer)                      }
       deriving (Show, Read)
instance Describable Item where
    name = itemName
    desc = itemDesc

data Slots = Slots {
                   }
           deriving (Show, Read)

emptySlots :: Slots
emptySlots = Slots {}

data Player = Player {playerName :: String
                     ,playerDesc :: String
                     ,hp :: Integer
                     ,weapons :: [Weapon]
                     ,items :: [Item]
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
       

-- Basically, the way records work they get the gamestate but the function never
-- really gets to look at the rest of the gamestate. This remedies that issue
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
addAction a = map (\(s,a') -> (s,a . a'))