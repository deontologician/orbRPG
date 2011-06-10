{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}




module Game.OrbRPG.Types where

import System.Console.Haskeline
import Control.Monad.Trans
import Control.Monad.State
import System.Random
import Control.Arrow (second)
import qualified Data.Map as M
import Control.Monad.Trans.Maybe()
import Data.Function
import Data.List (delete)
import Data.Maybe (fromJust)
import Control.Applicative ((<$>))
import System.Console.Haskeline
import Data.Record.Label


data Orb = Null | Black | White | P Primary | L Letter | E Element 
         | G God | D Demon | T Tech
                 deriving (Ord, Eq, Read, Show)


data Primary = Red | Green  | Blue 
               deriving (Ord, Eq, Read, Show, Enum, Bounded)

data Letter = Lambda | Mu | Omega 
            deriving (Ord, Eq, Read, Show, Enum, Bounded)

data Element = Deuterium | Erbium | Cesium
             deriving (Ord, Eq, Read, Show, Enum, Bounded)

data God = Nyx | Hypnos | Thanatos
         deriving (Ord, Eq, Read, Show, Enum, Bounded)

data Demon = Mammon | Asmodeus | Belial
           deriving (Ord, Eq, Read, Show, Enum, Bounded)

data Tech = Piston | Transistor | Gear
          deriving (Ord, Eq, Read, Show, Enum, Bounded)

-- A function to get one of the basics from an Int
getBasic :: Int -> Orb
getBasic = ([Null,White, Black, P Red, L Lambda, E Deuterium, G Nyx,
                 D Mammon, T Piston] !!)


instance Bounded Orb where
    minBound = Null
    maxBound = T Gear

instance Enum Orb where
    fromEnum Null  = 0
    fromEnum Black = 1
    fromEnum White = 2
    fromEnum (P p) = 3 + fromEnum p
    fromEnum (L l) = 6 + fromEnum l
    fromEnum (E e) = 9 + fromEnum e
    fromEnum (G g) = 12 + fromEnum g
    fromEnum (D d) = 15 + fromEnum d
    fromEnum (T t) = 18 + fromEnum t

    toEnum 0 = Null
    toEnum 1 = Black
    toEnum 2 = White
    toEnum x | x > 2 && x < 6 = P $ toEnum (x - 3)
             | x > 5 && x < 9 = L $ toEnum (x - 6)
             | x > 8 && x < 12 = E $ toEnum (x - 9)
             | x > 11 && x < 15 = G $ toEnum (x - 12)
             | x > 14 && x < 18 = D $ toEnum (x - 15)
             | x > 17 && x < 21 = T $ toEnum (x - 18)
             | otherwise = error "Int out of range for Orb Enum"

-- | Genes of Orbs

-- | Objects which can be named and examined.
class Describable d where
    name :: d -> String
    desc :: d -> String

-- | The simplest possible implementation of a Describable
type NameDesc = (String,String)

instance Describable NameDesc where
    name = fst
    desc = snd

-- | Energy types. Used to classify damage and each having a different property
data EnergyType = Abstract | Symbol | Physical | Spiritual | Regress | Progress
                deriving (Show,Read)

-- | Descriptions for the different Energy types
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

instance Describable EnergyType where
    name = show
    desc = energyDesc

-- | A basic unit of damage. The amount and the type
data Damage = Dmg { _dmgAmt :: Int
                  , _dmgType :: EnergyType }

data Player = Player {_playerName :: String
                     ,_playerDesc :: String
                     ,_hp :: Integer
                     ,_weapons :: [Object]
                     ,_inv :: [Object]
                     }
            deriving (Show, Read)
instance Describable Player where
    name = setL playerName
    desc = setL playerDesc

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



getInputLine' :: String -> RPG (Maybe String)
getInputLine' = lift . getInputLine

output :: String -> RPG ()
output = lift . outputStrLn

-- What do we need for Game state?
-- Two players.
-- That's it? Environmental effects? Maybe later.
-- Some kind of representation of the game
data GameState = GameState { _you     :: Player
                           ,_enemy    :: Maybe Player
                           ,_scrn     :: GameState -> NameDesc
    -- A list of valid inputs and the actions to perform for each of them
                           ,_parser   :: String -> Maybe Action
                           ,_response :: Maybe String
                           ,_seed     :: StdGen
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


-- | Directions in which the player can travel between locations.
data Direction = North    -- ^ north
               | South   -- ^ south
               | East    -- ^ east
               | West    -- ^ west
               | NE   -- ^ northeast
               | SE   -- ^ southeast
               | SW   -- ^ southwest
               | NW   -- ^ northwest
               | Down    -- ^ down
               | Up    -- ^ up
               | In    -- ^ in
               | Out    -- ^ out
  deriving (Read,Show,Eq,Ord,Enum)

-- | Convert a 'Direction' to a 'String'.
dirToStr :: Direction -> String
dirToStr N  = "north"
dirToStr S  = "south"
dirToStr E  = "east"
dirToStr W  = "west"
dirToStr NE = "northeast"
dirToStr SE = "southeast"
dirToStr SW = "southwest"
dirToStr NW = "northwest"
dirToStr D  = "down"
dirToStr U  = "up"
dirToStr I  = "in"
dirToStr O  = "out"

-- | The type by which the player's score is represented.
type Score = Int

-- | A type to encapsulate commands issued by the player.
data Command = Cmd String | Dir Direction
  deriving (Read,Show,Eq,Ord)

type ActionMap = M.Map Command (Adv ())
type ActionList = [(Command, Adv ())]

-- | A tag to denote different kinds of 'Object's.
data ObjectType = Location | AnOrb | Living
                deriving(Show, Read, Eq)

-- | The basic building block of adventure worlds.  'Object's are used
--   to represent locations, things, and characters.
--
--   'Object's are keyed on the 'name' field, which is used to
--   implement both equality and ordering for 'Object's.
data Object = Obj { _objname     :: String
                  , _objdesc     :: String
                  , _objType     :: ObjectType
                  , _actions     :: ActionMap
                  , _orbType     :: Orb
                  , _children    :: [Object]
                  , _aliases     :: [String]
                  }
              deriving(Show,Read, Eq)

-- | A default, empty object.
emptyThing :: Object
emptyThing = Obj { _objname     = ""
                 , _objdesc     = ""
                 , _objType     = AnOrb
                 , _actions     = M.empty
                 , _orbType     = Null
                 , _children    = []
                 , _aliases     = []
                 }

-- | A convenience alias.  At some point in the future, the 'Object' type
--   may get tagged with a phantom type so these types can be distinguished
--   at compile-time.
type Thing = Object

-- | Convenience alias for 'Object'.  See note about phantom types above.
type Location = Object

-- | Convenience alias for 'Object'.  See note about phantom types above.
type Character = Object

data Target a = Target a
              | Unknown String
              | None

handleTarget :: b -> (String -> b) -> (a -> b) -> Target a -> b
handleTarget b _ _ None        = b
handleTarget _ f _ (Unknown s) = f s
handleTarget _ _ f (Target a)  = f a

targetToMaybe :: Target a -> Maybe a
targetToMaybe = handleTarget Nothing (const Nothing) Just

instance Functor Target where
  fmap f (Target x)  = Target (f x)
  fmap _ (Unknown s) = Unknown s
  fmap _ None        = None

-- | A record type to store the game's persistent (mutable) state.
data GameState = GS { _dirO           :: Target Object -- ^ direct object
                    , _indO           :: Target Object -- ^ indirect object
                    , _loc            :: Object        -- ^ current location
                    , _childMap       :: M.Map String [Object]
                    -- ^ a map with the current children of each object
                    , _visited        :: M.Map String Object
                    -- ^ visited locations
                    , _globalActions  :: ActionMap     -- ^ global actions
                    , _config         :: GameConfig
                    -- ^ configuration options
                    }

-- | Configuration record used for parameterizing a game.
data GameConfig = GC { _newGlobalActions      :: ActionMap
                     , _overrideGlobalActions :: Bool
                     , _initialInventory      :: [Object]
                     , _startLoc              :: Object
                     }

data AdvCtrl a = Continue a
               | Exit
               | Fail

instance Functor AdvCtrl where
  fmap f (Continue x) = Continue (f x)
  fmap _ Exit = Exit
  fmap _ Fail = Fail

fromAdvCtrl :: a -> AdvCtrl a -> a
fromAdvCtrl _ (Continue x) = x
fromAdvCtrl x _ = x

newtype AdvCtrlT m a = AdvCtrlT { runAdvCtrlT :: m (AdvCtrl a) }

class (Monad m) => MonadAdvCtrl m where
  exit :: m ()
  cond :: Bool -> m ()
  try :: m a -> m ()

instance Functor m => Functor (AdvCtrlT m) where
  fmap f x = AdvCtrlT $ fmap (fmap f) $ runAdvCtrlT x

instance Monad m => Monad (AdvCtrlT m) where
  return  = AdvCtrlT . return . Continue
  x >>= f = AdvCtrlT $ do m <- runAdvCtrlT x
                          case m of
                            (Continue a) -> runAdvCtrlT (f a)
                            Fail         -> return Fail
                            Exit         -> return Exit
  fail _  = AdvCtrlT $ return Fail

instance (Monad m) => MonadAdvCtrl (AdvCtrlT m) where
  exit       = AdvCtrlT $ return Exit
  cond True  = return ()
  cond False = fail ""
  try  x     = AdvCtrlT $ do m <- runAdvCtrlT x
                             case m of
                               Continue _ -> return $ Continue ()
                               Fail       -> return $ Continue ()
                               Exit       -> return Exit

instance MonadTrans AdvCtrlT where
  lift m = AdvCtrlT $ m >>= return . Continue

instance MonadState s m => MonadState s (AdvCtrlT m) where
  get = lift get
  put = lift . put

instance MonadIO m => MonadIO (AdvCtrlT m) where
  liftIO = lift . liftIO

-- | The 'Adv' monad, which consists of a State monad of 'GameState'
--   layered on top of 'IO', topped off with a custom monad
--   transformer 'AdvCtrlT' expressing the necessary control flow.
newtype Adv a = Adv (AdvCtrlT (StateT GameState (InputT IO)) a)
  deriving (Monad, Functor, MonadState GameState, MonadIO, MonadAdvCtrl)

-- | Run an action in the 'Adv' monad given an initial 'GameState',
--   resulting in an 'IO' action.
runAdv :: Adv () -> GameState -> IO ()
runAdv (Adv adv) gameState = runInputT defaultSettings $
                             fromAdvCtrl () <$> evalStateT (runAdvCtrlT adv) gameState

-- This must be here since it is where TH generates the fclabels
mkLabels [''Object, ''GameState, ''GameConfig, ''Damage, ''Player]

instance Eq Object where
  (==) = (==) `on` getL name

instance Ord Object where
  (<=) = (<=) `on` getL name

-- | A default, empty location.
emptyLoc :: Object
emptyLoc = setL objType Location emptyThing

-- | A default, empty character.
emptyCharacter :: Object
emptyCharacter = setL objType Character emptyThing

-- | String ID for the special \'inventory\' object.
inventoryID :: String
inventoryID = "__INVENTORY"

-- | An empty game state.
emptyGameState :: GameState
emptyGameState = G { _score         = 0
                   , _dirO          = None
                   , _indO          = None
                   , _loc           = emptyLoc
                   , _childMap      = M.empty
                   , _visited       = M.empty
                   , _globalActions = M.empty
                   , _config        = emptyConfig
                   }

-- | A default, empty game configuration.
emptyConfig :: GameConfig
emptyConfig = GC { _newGlobalActions      = M.empty
                 , _overrideGlobalActions = False
                 , _initialInventory      = []
                 , _startLoc              = emptyLoc
                 }

applyConfig :: GameConfig -> Adv ()
applyConfig gc = do
  (if getL overrideGlobalActions gc
     then setS globalActions
     else addGlobalActions) $ getL newGlobalActions gc
  modifyInv (++ (getL initialInventory gc))
  setS config gc

setS :: (MonadState s m) => s :-> a -> a -> m ()
setS r x = modify (setL r x)

updateS :: (MonadState s m) => s :-> a -> (a -> a) -> m ()
updateS r f = modify (modL r f)

-- | Get the player's current inventory.
inventory :: Adv [Object]
inventory = (fromJust . M.lookup inventoryID) <$> gets (getL childMap)

-- | Modify the user's inventory with a function.
modifyInv :: ([Object] -> [Object]) -> Adv ()
modifyInv f = updateS childMap (M.adjust f inventoryID)

-- | Modify the child map by creating a new object with a given parent.
addObject :: Object    -- ^ The object to create.
          -> Object    -- ^ The parent object under which to create
                       --   the new object.
          -> Adv ()
addObject o l = updateS childMap (M.adjust (o:) (getL name l))

-- | Modify the child map by creating an object at the current
--   location.
addObjectHere :: Object -> Adv ()
addObjectHere o = gets (getL loc) >>= addObject o

-- | Modify the child map by deleting an object from a given parent.
delObject :: Object     -- ^ The object to delete.
          -> Object     -- ^ The parent object from which to delete.
          -> Adv ()
delObject o l = updateS childMap (M.adjust (delete o) (getL name l))

-- | Modify the child map by deleting an object from the current
--   location.
delObjectHere :: Object -> Adv ()
delObjectHere o = gets (getL loc) >>= delObject o

-- | Add a location to the 'visited' map.
visit :: Object -> Adv ()
visit o = updateS visited (M.insert (getL name o) o)

addGlobalActions :: ActionMap -> Adv ()
addGlobalActions acts = updateS globalActions (flip M.union acts)

-- | Get a haskeline edited prompt
getLineEdited :: String -> Adv (Maybe String)
getLineEdited = Adv . lift . lift . getInputLine

-- | A lifted version of outputStrLn
outputStrLn :: String -> Adv ()
outputStrLn = Adv . lift . lift . System.Console.Haskeline.outputStrLn

-- | A lifted version of outputStr
outputStr :: String -> Adv ()
outputStr = Adv . lift . lift . System.Console.Haskeline.outputStr