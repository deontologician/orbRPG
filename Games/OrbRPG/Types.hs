{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

module Types where


import Combine
import System.Console.Haskeline
import Control.Monad.Trans
import Control.Monad.State

class Describable d where
    name :: d -> String
    desc :: d -> String

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
                           ,enemy :: Player
                           ,scrn :: GameState -> String
    -- A list of valid inputs and the actions to perform for each of them
                           ,parser :: String -> Action
}

setYou :: Player -> Action
setYou pl gs = gs{you = pl}

setEnemy :: Player -> Action
setEnemy pl gs = gs{enemy = pl}

setScreen :: (GameState -> String) -> Action
setScreen scr gs = gs{scrn = scr}

setParser :: (String -> Action) -> Action
setParser ps gs = gs{parser = ps}

-- Basically, the way records work they get the gamestate but the function never
-- really gets to look at the rest of the gamestate. This remedies that issue
screen :: GameState -> String
screen gs = scrn gs gs

-- What is an action? Well I'll tell you.
type Action = GameState -> GameState
-- Hey, actions are composable, awesome.

-- takes a list of tokens to actions and a default action and just does a lookup
simpleparser' :: Action -> [(String,Action)]-> String -> Action
simpleparser' def cmds str = maybe def id (lookup str cmds)

-- Even simpler...
simpleparser :: [(String,Action)] -> String -> Action
simpleparser = simpleparser' id

--Simplest!
idParser :: String -> Action
idParser = const id

nameList :: (Describable a) => [a] -> [(String,a)]
nameList lst = zip (map name lst) lst

addAction :: Action -> [(String,Action)] -> [(String,Action)]
addAction a = map (\(s,a') -> (s,a . a'))