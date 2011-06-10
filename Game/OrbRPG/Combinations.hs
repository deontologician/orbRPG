{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverlappingInstances #-}

module Game.OrbRPG.Combinations where


import Game.OrbRPG.Types

import System.Random

-- Main combination function
(@>>) :: Orb -> Orb -> Orb

-- Null Combinations
_    @>> Null = Null
Null @>> Black = White
Null @>> White = Black
Null @>> _     = Null

-- White + White
White @>> White = P Red

-- White + Black
White @>> Black = P Red

-- White + Primary
White @>> P Red = L Lambda
White @>> P Green = L Mu
White @>> P Blue = L Lambda

-- White + Letter
White @>> L Lambda = G Nyx
White @>> L Mu = E Cesium
White @>> L Omega = P Blue

-- White + Element
White @>> E Deuterium = L Lambda
White @>> E Erbium = P Red
White @>> E Cesium = L Mu

-- White + God
White @>> G Nyx = E Deuterium
White @>> G Hypnos = P Red
White @>> G Thanatos = P Green

-- White + Demon
White @>> D Mammon = L Omega
White @>> D Asmodeus = E Deuterium
White @>> D Belial = L Omega

-- White + Tech
White @>> T Piston = G Nyx
White @>> T Transistor = L Omega
White @>> T Gear = E Deuterium

-- Black + White
Black @>> White = E Deuterium

-- Black + Black
Black @>> Black = L Lambda

-- Black + Primary
Black @>> P Red = L Mu
Black @>> P Green = P Red
Black @>> P Blue = E Erbium

-- Black + Letter
Black @>> L Lambda = G Nyx
Black @>> L Mu = G Nyx
Black @>> L Omega = White

-- Black + Element
Black @>> E Deuterium = P Red
Black @>> E Erbium = P Red
Black @>> E Cesium = White

-- Black + God
Black @>> G Nyx = E Erbium
Black @>> G Hypnos = E Erbium
Black @>> G Thanatos = L Lambda

-- Black + Demon
Black @>> D Mammon = P Red
Black @>> D Asmodeus = P Blue
Black @>> D Belial = P Red

-- Black + Tech
Black @>> T Piston = D Asmodeus
Black @>> T Transistor = E Cesium
Black @>> T Gear = L Mu

-- Primary + White
P Red @>> White = L Omega
P Green @>> White = E Deuterium
P Blue @>> White = E Deuterium

-- Primary + Black
P Red @>> Black = L Mu
P Green @>> Black = E Deuterium
P Blue @>> Black = E Cesium

-- Primary + Primary
P Red @>> P Red = E Deuterium
P Red @>> P Green = L Lambda
P Red @>> P Blue = E Deuterium
P Green @>> P Red = L Lambda
P Green @>> P Green = E Erbium
P Green @>> P Blue = P Red
P Blue @>> P Red = P Red
P Blue @>> P Green = White
P Blue @>> P Blue = P Blue

-- Primary + Letter
P Red @>> L Lambda = G Nyx
P Red @>> L Mu = E Deuterium
P Red @>> L Omega = P Blue
P Green @>> L Lambda = E Deuterium
P Green @>> L Mu = L Lambda
P Green @>> L Omega = L Lambda
P Blue @>> L Lambda = E Deuterium
P Blue @>> L Mu = E Deuterium
P Blue @>> L Omega = P Green

-- Primary + Element
P Red @>> E Deuterium = L Lambda
P Red @>> E Erbium = P Red
P Red @>> E Cesium = L Lambda
P Green @>> E Deuterium = L Lambda
P Green @>> E Erbium = P Green
P Green @>> E Cesium = L Omega
P Blue @>> E Deuterium = P Red
P Blue @>> E Erbium = E Erbium
P Blue @>> E Cesium = D Mammon

-- Primary + God
P Red @>> G Nyx = White
P Red @>> G Hypnos = P Green
P Red @>> G Thanatos = G Thanatos
P Green @>> G Nyx = L Omega
P Green @>> G Hypnos = D Mammon
P Green @>> G Thanatos = D Asmodeus
P Blue @>> G Nyx = E Deuterium
P Blue @>> G Hypnos = E Deuterium
P Blue @>> G Thanatos = L Mu

-- Primary + Demon
P Red @>> D Mammon = T Piston
P Red @>> D Asmodeus = P Blue
P Red @>> D Belial = L Lambda
P Green @>> D Mammon = E Deuterium
P Green @>> D Asmodeus = G Nyx
P Green @>> D Belial = P Red
P Blue @>> D Mammon = L Mu
P Blue @>> D Asmodeus = G Hypnos
P Blue @>> D Belial = E Deuterium

-- Primary + Tech
P Red @>> T Piston = D Asmodeus
P Red @>> T Transistor = E Deuterium
P Red @>> T Gear = G Hypnos
P Green @>> T Piston = L Lambda
P Green @>> T Transistor = P Green
P Green @>> T Gear = D Belial
P Blue @>> T Piston = G Thanatos
P Blue @>> T Transistor = P Red
P Blue @>> T Gear = T Transistor

-- Letter + White
L Lambda @>> White = L Mu
L Mu @>> White = P Blue
L Omega @>> White = G Nyx

-- Letter + Black
L Lambda @>> Black = P Red
L Mu @>> Black = White
L Omega @>> Black = P Red

-- Letter + Primary
L Lambda @>> P Red = E Deuterium
L Lambda @>> P Green = G Nyx
L Lambda @>> P Blue = L Lambda
L Mu @>> P Red = P Red
L Mu @>> P Green = G Nyx
L Mu @>> P Blue = P Red
L Omega @>> P Red = P Green
L Omega @>> P Green = L Lambda
L Omega @>> P Blue = E Erbium

-- Letter + Letter
L Lambda @>> L Lambda = L Lambda
L Lambda @>> L Mu = E Cesium
L Lambda @>> L Omega = L Omega
L Mu @>> L Lambda = G Hypnos
L Mu @>> L Mu = L Lambda
L Mu @>> L Omega = L Mu
L Omega @>> L Lambda = G Nyx
L Omega @>> L Mu = P Green
L Omega @>> L Omega = P Red

-- Letter + Element
L Lambda @>> E Deuterium = P Red
L Lambda @>> E Erbium = E Deuterium
L Lambda @>> E Cesium = L Lambda
L Mu @>> E Deuterium = G Hypnos
L Mu @>> E Erbium = L Omega
L Mu @>> E Cesium = G Nyx
L Omega @>> E Deuterium = P Green
L Omega @>> E Erbium = Black
L Omega @>> E Cesium = D Mammon

-- Letter + God
L Lambda @>> G Nyx = L Lambda
L Lambda @>> G Hypnos = D Asmodeus
L Lambda @>> G Thanatos = E Deuterium
L Mu @>> G Nyx = E Erbium
L Mu @>> G Hypnos = E Deuterium
L Mu @>> G Thanatos = E Deuterium
L Omega @>> G Nyx = D Belial
L Omega @>> G Hypnos = White
L Omega @>> G Thanatos = D Mammon

-- Letter + Demon
L Lambda @>> D Mammon = D Asmodeus
L Lambda @>> D Asmodeus = L Lambda
L Lambda @>> D Belial = L Lambda
L Mu @>> D Mammon = P Green
L Mu @>> D Asmodeus = P Red
L Mu @>> D Belial = D Mammon
L Omega @>> D Mammon = T Piston
L Omega @>> D Asmodeus = L Mu
L Omega @>> D Belial = T Transistor

-- Letter + Tech
L Lambda @>> T Piston = P Red
L Lambda @>> T Transistor = L Lambda
L Lambda @>> T Gear = E Deuterium
L Mu @>> T Piston = P Blue
L Mu @>> T Transistor = P Red
L Mu @>> T Gear = D Asmodeus
L Omega @>> T Piston = White
L Omega @>> T Transistor = L Lambda
L Omega @>> T Gear = White

-- Element + White
E Deuterium @>> White = L Mu
E Erbium @>> White = Black
E Cesium @>> White = E Erbium

-- Element + Black
E Deuterium @>> Black = L Lambda
E Erbium @>> Black = L Mu
E Cesium @>> Black = D Mammon

-- Element + Primary
E Deuterium @>> P Red = G Nyx
E Deuterium @>> P Green = L Mu
E Deuterium @>> P Blue = G Nyx
E Erbium @>> P Red = L Mu
E Erbium @>> P Green = White
E Erbium @>> P Blue = E Deuterium
E Cesium @>> P Red = Black
E Cesium @>> P Green = L Lambda
E Cesium @>> P Blue = P Red

-- Element + Letter
E Deuterium @>> L Lambda = P Blue
E Deuterium @>> L Mu = L Lambda
E Deuterium @>> L Omega = E Erbium
E Erbium @>> L Lambda = E Cesium
E Erbium @>> L Mu = L Lambda
E Erbium @>> L Omega = L Mu
E Cesium @>> L Lambda = P Green
E Cesium @>> L Mu = L Mu
E Cesium @>> L Omega = L Lambda

-- Element + Element
E Deuterium @>> E Deuterium = E Erbium
E Deuterium @>> E Erbium = Black
E Deuterium @>> E Cesium = G Nyx
E Erbium @>> E Deuterium = L Mu
E Erbium @>> E Erbium = L Mu
E Erbium @>> E Cesium = P Red
E Cesium @>> E Deuterium = L Lambda
E Cesium @>> E Erbium = P Red
E Cesium @>> E Cesium = P Red

-- Element + God
E Deuterium @>> G Nyx = E Cesium
E Deuterium @>> G Hypnos = P Green
E Deuterium @>> G Thanatos = P Green
E Erbium @>> G Nyx = L Mu
E Erbium @>> G Hypnos = G Thanatos
E Erbium @>> G Thanatos = P Red
E Cesium @>> G Nyx = D Asmodeus
E Cesium @>> G Hypnos = G Nyx
E Cesium @>> G Thanatos = P Green

-- Element + Demon
E Deuterium @>> D Mammon = L Lambda
E Deuterium @>> D Asmodeus = E Deuterium
E Deuterium @>> D Belial = D Asmodeus
E Erbium @>> D Mammon = Black
E Erbium @>> D Asmodeus = E Erbium
E Erbium @>> D Belial = D Mammon
E Cesium @>> D Mammon = E Erbium
E Cesium @>> D Asmodeus = G Thanatos
E Cesium @>> D Belial = E Deuterium

-- Element + Tech
E Deuterium @>> T Piston = Black
E Deuterium @>> T Transistor = P Red
E Deuterium @>> T Gear = G Nyx
E Erbium @>> T Piston = P Red
E Erbium @>> T Transistor = P Red
E Erbium @>> T Gear = E Deuterium
E Cesium @>> T Piston = P Red
E Cesium @>> T Transistor = Black
E Cesium @>> T Gear = E Deuterium

-- God + White
G Nyx @>> White = G Nyx
G Hypnos @>> White = P Red
G Thanatos @>> White = L Lambda

-- God + Black
G Nyx @>> Black = G Nyx
G Hypnos @>> Black = E Deuterium
G Thanatos @>> Black = P Red

-- God + Primary
G Nyx @>> P Red = D Mammon
G Nyx @>> P Green = Black
G Nyx @>> P Blue = L Lambda
G Hypnos @>> P Red = E Cesium
G Hypnos @>> P Green = G Nyx
G Hypnos @>> P Blue = D Asmodeus
G Thanatos @>> P Red = L Lambda
G Thanatos @>> P Green = L Lambda
G Thanatos @>> P Blue = P Red

-- God + Letter
G Nyx @>> L Lambda = T Transistor
G Nyx @>> L Mu = D Mammon
G Nyx @>> L Omega = White
G Hypnos @>> L Lambda = White
G Hypnos @>> L Mu = G Nyx
G Hypnos @>> L Omega = G Nyx
G Thanatos @>> L Lambda = D Belial
G Thanatos @>> L Mu = D Asmodeus
G Thanatos @>> L Omega = T Piston

-- God + Element
G Nyx @>> E Deuterium = E Deuterium
G Nyx @>> E Erbium = D Mammon
G Nyx @>> E Cesium = P Red
G Hypnos @>> E Deuterium = E Deuterium
G Hypnos @>> E Erbium = P Blue
G Hypnos @>> E Cesium = P Red
G Thanatos @>> E Deuterium = E Deuterium
G Thanatos @>> E Erbium = L Mu
G Thanatos @>> E Cesium = P Red

-- God + God
G Nyx @>> G Nyx = P Green
G Nyx @>> G Hypnos = E Deuterium
G Nyx @>> G Thanatos = L Lambda
G Hypnos @>> G Nyx = L Omega
G Hypnos @>> G Hypnos = E Deuterium
G Hypnos @>> G Thanatos = L Lambda
G Thanatos @>> G Nyx = L Lambda
G Thanatos @>> G Hypnos = D Mammon
G Thanatos @>> G Thanatos = L Mu

-- God + Demon
G Nyx @>> D Mammon = L Omega
G Nyx @>> D Asmodeus = P Red
G Nyx @>> D Belial = Black
G Hypnos @>> D Mammon = P Red
G Hypnos @>> D Asmodeus = T Transistor
G Hypnos @>> D Belial = G Nyx
G Thanatos @>> D Mammon = L Mu
G Thanatos @>> D Asmodeus = E Erbium
G Thanatos @>> D Belial = T Gear

-- God + Tech
G Nyx @>> T Piston = L Lambda
G Nyx @>> T Transistor = P Green
G Nyx @>> T Gear = T Transistor
G Hypnos @>> T Piston = P Red
G Hypnos @>> T Transistor = P Blue
G Hypnos @>> T Gear = P Blue
G Thanatos @>> T Piston = D Asmodeus
G Thanatos @>> T Transistor = G Nyx
G Thanatos @>> T Gear = E Erbium

-- Demon + White
D Mammon @>> White = G Hypnos
D Asmodeus @>> White = L Omega
D Belial @>> White = P Red

-- Demon + Black
D Mammon @>> Black = T Piston
D Asmodeus @>> Black = E Erbium
D Belial @>> Black = G Hypnos

-- Demon + Primary
D Mammon @>> P Red = L Omega
D Mammon @>> P Green = P Red
D Mammon @>> P Blue = P Blue
D Asmodeus @>> P Red = E Cesium
D Asmodeus @>> P Green = L Mu
D Asmodeus @>> P Blue = E Erbium
D Belial @>> P Red = G Nyx
D Belial @>> P Green = P Red
D Belial @>> P Blue = Black

-- Demon + Letter
D Mammon @>> L Lambda = P Red
D Mammon @>> L Mu = G Hypnos
D Mammon @>> L Omega = L Omega
D Asmodeus @>> L Lambda = E Erbium
D Asmodeus @>> L Mu = G Nyx
D Asmodeus @>> L Omega = G Nyx
D Belial @>> L Lambda = L Mu
D Belial @>> L Mu = D Asmodeus
D Belial @>> L Omega = P Red

-- Demon + Element
D Mammon @>> E Deuterium = L Mu
D Mammon @>> E Erbium = E Erbium
D Mammon @>> E Cesium = E Cesium
D Asmodeus @>> E Deuterium = P Green
D Asmodeus @>> E Erbium = P Red
D Asmodeus @>> E Cesium = G Nyx
D Belial @>> E Deuterium = G Nyx
D Belial @>> E Erbium = G Hypnos
D Belial @>> E Cesium = P Green

-- Demon + God
D Mammon @>> G Nyx = Black
D Mammon @>> G Hypnos = D Asmodeus
D Mammon @>> G Thanatos = P Green
D Asmodeus @>> G Nyx = D Belial
D Asmodeus @>> G Hypnos = D Asmodeus
D Asmodeus @>> G Thanatos = Black
D Belial @>> G Nyx = G Nyx
D Belial @>> G Hypnos = White
D Belial @>> G Thanatos = Black

-- Demon + Demon
D Mammon @>> D Mammon = G Thanatos
D Mammon @>> D Asmodeus = P Green
D Mammon @>> D Belial = L Mu
D Asmodeus @>> D Mammon = T Transistor
D Asmodeus @>> D Asmodeus = E Erbium
D Asmodeus @>> D Belial = P Red
D Belial @>> D Mammon = P Green
D Belial @>> D Asmodeus = G Thanatos
D Belial @>> D Belial = P Red

-- Demon + Tech
D Mammon @>> T Piston = L Lambda
D Mammon @>> T Transistor = E Cesium
D Mammon @>> T Gear = L Omega
D Asmodeus @>> T Piston = L Lambda
D Asmodeus @>> T Transistor = E Erbium
D Asmodeus @>> T Gear = D Mammon
D Belial @>> T Piston = L Mu
D Belial @>> T Transistor = L Lambda
D Belial @>> T Gear = L Omega

-- Tech + White
T Piston @>> White = L Mu
T Transistor @>> White = G Hypnos
T Gear @>> White = P Red

-- Tech + Black
T Piston @>> Black = L Mu
T Transistor @>> Black = G Nyx
T Gear @>> Black = E Cesium

-- Tech + Primary
T Piston @>> P Red = L Mu
T Piston @>> P Green = P Green
T Piston @>> P Blue = G Nyx
T Transistor @>> P Red = L Lambda
T Transistor @>> P Green = D Asmodeus
T Transistor @>> P Blue = L Mu
T Gear @>> P Red = L Omega
T Gear @>> P Green = T Piston
T Gear @>> P Blue = P Red

-- Tech + Letter
T Piston @>> L Lambda = G Nyx
T Piston @>> L Mu = E Erbium
T Piston @>> L Omega = L Lambda
T Transistor @>> L Lambda = L Mu
T Transistor @>> L Mu = G Nyx
T Transistor @>> L Omega = P Blue
T Gear @>> L Lambda = L Lambda
T Gear @>> L Mu = G Nyx
T Gear @>> L Omega = D Mammon

-- Tech + Element
T Piston @>> E Deuterium = L Omega
T Piston @>> E Erbium = L Omega
T Piston @>> E Cesium = L Mu
T Transistor @>> E Deuterium = E Cesium
T Transistor @>> E Erbium = G Nyx
T Transistor @>> E Cesium = D Asmodeus
T Gear @>> E Deuterium = L Lambda
T Gear @>> E Erbium = E Erbium
T Gear @>> E Cesium = P Blue

-- Tech + God
T Piston @>> G Nyx = G Hypnos
T Piston @>> G Hypnos = L Lambda
T Piston @>> G Thanatos = L Mu
T Transistor @>> G Nyx = G Hypnos
T Transistor @>> G Hypnos = L Lambda
T Transistor @>> G Thanatos = P Red
T Gear @>> G Nyx = T Piston
T Gear @>> G Hypnos = D Mammon
T Gear @>> G Thanatos = T Piston

-- Tech + Demon
T Piston @>> D Mammon = L Lambda
T Piston @>> D Asmodeus = G Nyx
T Piston @>> D Belial = E Deuterium
T Transistor @>> D Mammon = D Asmodeus
T Transistor @>> D Asmodeus = P Red
T Transistor @>> D Belial = L Mu
T Gear @>> D Mammon = T Transistor
T Gear @>> D Asmodeus = D Asmodeus
T Gear @>> D Belial = T Gear

-- Tech + Tech
T Piston @>> T Piston = E Cesium
T Piston @>> T Transistor = G Nyx
T Piston @>> T Gear = E Deuterium
T Transistor @>> T Piston = E Deuterium
T Transistor @>> T Transistor = E Deuterium
T Transistor @>> T Gear = White
T Gear @>> T Piston = G Nyx
T Gear @>> T Transistor = L Lambda
T Gear @>> T Gear = G Nyx


-- Finally done!

-- Expands a single element into its neighbors in their respective probabilities
-- This implicitly holds the intra Orb-type rarity
xpand :: (Enum a, Bounded a) => a -> [a]
xpand _ = [a',a',a',b,b,c]
    where
      a' = minBound
      b = succ a'
      c = succ b

xpand' :: (Orbable a, Enum a, Bounded a) => Int -> a ->  [Orb]
xpand' n = map o . concat . replicate n . xpand

-- These are the individual Orb type probabilities
pp :: [Orb]
pp = xpand' 12 (minBound :: Primary)
ll :: [Orb]
ll = xpand' 10 (minBound :: Letter)
ee :: [Orb]
ee = xpand' 8  (minBound :: Element)
gg :: [Orb]
gg = xpand' 6  (minBound :: God)
dd :: [Orb]
dd = xpand' 4  (minBound :: Demon)
tt :: [Orb]
tt = xpand' 2  (minBound :: Tech)

orbProbList :: [Orb]
orbProbList = concat [[White],[Black],pp,ll,ee,gg,dd,tt]

randElem :: (RandomGen g) => [a] -> g -> (a,g)
randElem as g = (as !! i,g')
    where
      len = length as
      (i,g') = randomR (0, len - 1) g
      
      

instance Random Primary where
    randomR (a,z) g = (toEnum (retval `mod` 3 ),g')
        where (retval,g') = randomR (fromEnum a ,fromEnum z ) g
    random = randElem $ xpand Red
instance Random Letter where
    randomR (a,z) g = (toEnum (retval `mod` 3 ),g')
        where (retval,g') = randomR (fromEnum a ,fromEnum z ) g
    random = randElem $ xpand Lambda
instance Random Element where
    randomR (a,z) g = (toEnum (retval `mod` 3 ),g')
        where (retval,g') = randomR (fromEnum a ,fromEnum z ) g
    random = randElem $ xpand Deuterium

instance Random God where
    randomR (a,z) g = (toEnum (retval `mod` 3 ),g')
        where (retval,g') = randomR (fromEnum a ,fromEnum z ) g
    random = randElem $ xpand Nyx
instance Random Demon where
    randomR (a,z) g = (toEnum (retval `mod` 3 ),g')
        where (retval,g') = randomR (fromEnum a ,fromEnum z ) g
    random = randElem $ xpand Mammon
instance Random Tech where
    randomR (a,z) g = (toEnum (retval `mod` 3 ),g')
        where (retval,g') = randomR (fromEnum a ,fromEnum z ) g
    random = randElem $ xpand Piston

instance Random Orb where
    randomR (a,z) g = (toEnum (retval `mod` 21 ),g')
        where (retval,g') = randomR (fromEnum a ,fromEnum z ) g
    random = randElem orbProbList

class Orbable a where
    o :: a -> Orb

instance Orbable Primary where
    o = P
instance Orbable Letter where
    o = L
instance Orbable Element where
    o = E
instance Orbable God where
    o = G
instance Orbable Demon where
    o = D
instance Orbable Tech where
    o = T
instance Orbable Orb where
    o = id


data OrbTree a = OrbNode {orb :: Orb
                         ,attr :: a
                         ,cLeft :: OrbTree a
                         ,cRight :: OrbTree a
                         } 
               | OrbLeaf {orb :: Orb
                         ,attr :: a}


combinate :: (Orb -> a -> a -> a) -> OrbTree a -> OrbTree a -> OrbTree a
combinate f xx yy = 
    OrbNode{orb = combined
           ,attr = f combined (attr xx) (attr yy)
           ,cLeft = xx
           ,cRight = yy}
    where 
      combined :: Orb
      combined = orb xx @>> orb yy

-- | For a given orb type, gives the calculation of its damage types and points
--   towards that type
baseEnergy :: Orb -> [Damage]
baseEnergy = doitup
    where 
      go :: (Enum a) => a -> EnergyType -> EnergyType -> [Damage]
      go a e1 e2 = [Dmg (fromEnum a) e1, Dmg (fromEnum a) e2]
      doitup Null = []
      doitup White = []
      doitup Black = []
      doitup (P p) = go p Abstract Physical
      doitup (L l) = go l Abstract Symbol
      doitup (E e) = go e Physical Symbol
      doitup (G g) = go g Spiritual Progress
      doitup (D d) = go d Spiritual Regress
      doitup (T t) = go t Progress Regress

