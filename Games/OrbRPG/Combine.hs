{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
module Combine where

import System.Random

data Orb = Null | Black | White | P Primary | L Letter | G God 
               | D Demon | E Element | T Tech
                 deriving (Ord, Eq, Read, Show)

instance Bounded Orb where
    minBound = Null
    maxBound = T Gear

instance Enum Orb where
    fromEnum Null = 0
    fromEnum Black = 1
    fromEnum White = 2
    fromEnum (P p) = 3 + fromEnum p
    fromEnum (L l) = 6 + fromEnum l
    fromEnum (G g) = 9 + fromEnum g
    fromEnum (D d) = 12 + fromEnum d
    fromEnum (E e) = 15 + fromEnum e
    fromEnum (T t) = 18 + fromEnum t

    toEnum 0 = Null
    toEnum 1 = Black
    toEnum 2 = White
    toEnum x | x > 2 && x < 6 = P $ toEnum (x - 3)
             | x > 5 && x < 9 = L $ toEnum (x - 6)
             | x > 8 && x < 12 = G $ toEnum (x - 9)
             | x > 11 && x < 15 = D $ toEnum (x - 12)
             | x > 14 && x < 18 = E $ toEnum (x - 15)
             | x > 17 && x < 21 = T $ toEnum (x - 18)
             | otherwise = error "Int out of range for Orb Enum"

-- A function to get one of the basics from an Int
getBasic :: Int -> Orb
getBasic = ([Null,White,Black,P Red,L Lambda,G Nyx,
                 D Mammon,E Deuterium,T Piston] !!)

data Primary = Red | Green  | Blue 
               deriving (Ord, Eq, Read, Show, Enum, Bounded)

data Letter = Lambda | Mu | Omega 
            deriving (Ord, Eq, Enum, Bounded)

data God = Nyx | Hypnos | Thanatos
         deriving (Ord, Eq, Read, Show, Enum, Bounded)

data Demon = Mammon | Asmodeus | Belial
           deriving (Ord, Eq, Read, Show, Enum, Bounded)

data Element = Deuterium | Erbium | Cesium
             deriving (Ord, Eq, Read, Show, Enum, Bounded)

data Tech = Piston | Transistor | Gear
          deriving (Ord, Eq, Read, Show, Enum, Bounded)


-- The Show and Read instances for Letters are a bit different
instance Show Letter where
    show Lambda = "[(λ)]"
    show Mu     = "[(μ)]"
    show Omega  = "[(Ω)]"

instance Read Letter where
    readsPrec _ value = 
        tryParse [("[(λ)]",Lambda),("[(μ)]",Mu),("[(Ω)]",Omega),
                 ("Lambda",Lambda),("Mu",Mu),("Omega",Omega)]
        where tryParse [] = []
              tryParse ((attempt,result):xs) =
                  if (take (length attempt) value) == attempt
                  then [(result, drop (length attempt) value)]
                  else tryParse xs

-- Main combination function
(#>>) :: Orb -> Orb -> Orb

-- Null Combinations
_    #>> Null = Null
Null #>> Black = White
Null #>> White = Black
Null #>> _     = Null

-- White + White
White #>> White = E Erbium

-- White + Black
White #>> Black = D Mammon

-- White + Primary
White #>> P Red = E Erbium
White #>> P Green = G Nyx
White #>> P Blue = L Lambda

-- White + Letter
White #>> L Lambda = L Mu
White #>> L Mu = P Green
White #>> L Omega = P Red

-- White + Element
White #>> E Deuterium = L Lambda
White #>> E Erbium = L Omega
White #>> E Cesium = L Lambda

-- White + God
White #>> G Nyx = G Nyx
White #>> G Hypnos = E Deuterium
White #>> G Thanatos = E Erbium

-- White + Demon
White #>> D Mammon = L Mu
White #>> D Asmodeus = E Erbium
White #>> D Belial = L Lambda

-- White + Tech
White #>> T Piston = L Lambda
White #>> T Transistor = G Hypnos
White #>> T Gear = E Deuterium

-- Black + White
Black #>> White = P Red

-- Black + Black
Black #>> Black = L Mu

-- Black + Primary
Black #>> P Red = P Green
Black #>> P Green = E Deuterium
Black #>> P Blue = L Lambda

-- Black + Letter
Black #>> L Lambda = G Hypnos
Black #>> L Mu = L Lambda
Black #>> L Omega = P Red

-- Black + Element
Black #>> E Deuterium = L Mu
Black #>> E Erbium = P Red
Black #>> E Cesium = G Nyx

-- Black + God
Black #>> G Nyx = G Hypnos
Black #>> G Hypnos = L Lambda
Black #>> G Thanatos = D Mammon

-- Black + Demon
Black #>> D Mammon = L Lambda
Black #>> D Asmodeus = L Mu
Black #>> D Belial = G Hypnos

-- Black + Tech
Black #>> T Piston = E Deuterium
Black #>> T Transistor = P Green
Black #>> T Gear = L Mu

-- Primary + White
P Red #>> White = L Lambda
P Green #>> White = P Blue
P Blue #>> White = E Deuterium

-- Primary + Black
P Red #>> Black = G Nyx
P Green #>> Black = E Erbium
P Blue #>> Black = E Deuterium

-- Primary + Primary
P Red #>> P Red = T Transistor
P Red #>> P Green = D Asmodeus
P Red #>> P Blue = D Asmodeus
P Green #>> P Red = G Nyx
P Green #>> P Green = G Thanatos
P Green #>> P Blue = P Green
P Blue #>> P Red = T Piston
P Blue #>> P Green = E Cesium
P Blue #>> P Blue = L Omega

-- Primary + Letter
P Red #>> L Lambda = P Red
P Red #>> L Mu = P Green
P Red #>> L Omega = P Red
P Green #>> L Lambda = P Red
P Green #>> L Mu = P Red
P Green #>> L Omega = Black
P Blue #>> L Lambda = E Erbium
P Blue #>> L Mu = D Mammon
P Blue #>> L Omega = G Hypnos

-- Primary + Element
P Red #>> E Deuterium = D Asmodeus
P Red #>> E Erbium = T Piston
P Red #>> E Cesium = E Deuterium
P Green #>> E Deuterium = P Green
P Green #>> E Erbium = L Lambda
P Green #>> E Cesium = L Lambda
P Blue #>> E Deuterium = E Deuterium
P Blue #>> E Erbium = P Red
P Blue #>> E Cesium = P Green

-- Primary + God
P Red #>> G Nyx = L Lambda
P Red #>> G Hypnos = P Red
P Red #>> G Thanatos = G Nyx
P Green #>> G Nyx = E Cesium
P Green #>> G Hypnos = P Red
P Green #>> G Thanatos = P Red
P Blue #>> G Nyx = G Hypnos
P Blue #>> G Hypnos = White
P Blue #>> G Thanatos = E Deuterium

-- Primary + Demon
P Red #>> D Mammon = L Lambda
P Red #>> D Asmodeus = P Red
P Red #>> D Belial = P Red
P Green #>> D Mammon = E Cesium
P Green #>> D Asmodeus = White
P Green #>> D Belial = E Deuterium
P Blue #>> D Mammon = P Red
P Blue #>> D Asmodeus = L Mu
P Blue #>> D Belial = P Green

-- Primary + Tech
P Red #>> T Piston = E Deuterium
P Red #>> T Transistor = P Red
P Red #>> T Gear = D Asmodeus
P Green #>> T Piston = P Blue
P Green #>> T Transistor = D Mammon
P Green #>> T Gear = L Lambda
P Blue #>> T Piston = E Deuterium
P Blue #>> T Transistor = L Omega
P Blue #>> T Gear = G Thanatos

-- Letter + White
L Lambda #>> White = E Deuterium
L Mu #>> White = T Piston
L Omega #>> White = T Gear

-- Letter + Black
L Lambda #>> Black = P Green
L Mu #>> Black = D Asmodeus
L Omega #>> Black = E Deuterium

-- Letter + Primary
L Lambda #>> P Red = E Deuterium
L Lambda #>> P Green = P Blue
L Lambda #>> P Blue = P Green
L Mu #>> P Red = L Mu
L Mu #>> P Green = D Mammon
L Mu #>> P Blue = P Red
L Omega #>> P Red = P Red
L Omega #>> P Green = L Lambda
L Omega #>> P Blue = E Deuterium

-- Letter + Letter
L Lambda #>> L Lambda = P Red
L Lambda #>> L Mu = D Asmodeus
L Lambda #>> L Omega = L Mu
L Mu #>> L Lambda = E Deuterium
L Mu #>> L Mu = L Lambda
L Mu #>> L Omega = G Nyx
L Omega #>> L Lambda = P Red
L Omega #>> L Mu = P Red
L Omega #>> L Omega = E Cesium

-- Letter + Element
L Lambda #>> E Deuterium = D Asmodeus
L Lambda #>> E Erbium = L Mu
L Lambda #>> E Cesium = E Deuterium
L Mu #>> E Deuterium = P Green
L Mu #>> E Erbium = E Deuterium
L Mu #>> E Cesium = D Belial
L Omega #>> E Deuterium = L Lambda
L Omega #>> E Erbium = E Cesium
L Omega #>> E Cesium = G Nyx

-- Letter + God
L Lambda #>> G Nyx = P Red
L Lambda #>> G Hypnos = L Omega
L Lambda #>> G Thanatos = P Green
L Mu #>> G Nyx = D Mammon
L Mu #>> G Hypnos = P Red
L Mu #>> G Thanatos = L Lambda
L Omega #>> G Nyx = G Hypnos
L Omega #>> G Hypnos = T Transistor
L Omega #>> G Thanatos = E Cesium

-- Letter + Demon
L Lambda #>> D Mammon = G Nyx
L Lambda #>> D Asmodeus = P Blue
L Lambda #>> D Belial = Black
L Mu #>> D Mammon = D Mammon
L Mu #>> D Asmodeus = D Belial
L Mu #>> D Belial = E Deuterium
L Omega #>> D Mammon = L Mu
L Omega #>> D Asmodeus = P Green
L Omega #>> D Belial = P Red

-- Letter + Tech
L Lambda #>> T Piston = G Hypnos
L Lambda #>> T Transistor = G Nyx
L Lambda #>> T Gear = L Mu
L Mu #>> T Piston = E Cesium
L Mu #>> T Transistor = P Green
L Mu #>> T Gear = White
L Omega #>> T Piston = G Hypnos
L Omega #>> T Transistor = P Red
L Omega #>> T Gear = G Hypnos

-- Element + White
E Deuterium #>> White = L Lambda
E Erbium #>> White = Black
E Cesium #>> White = E Deuterium

-- Element + Black
E Deuterium #>> Black = D Belial
E Erbium #>> Black = L Omega
E Cesium #>> Black = P Blue

-- Element + Primary
E Deuterium #>> P Red = E Deuterium
E Deuterium #>> P Green = L Lambda
E Deuterium #>> P Blue = P Green
E Erbium #>> P Red = P Green
E Erbium #>> P Green = P Green
E Erbium #>> P Blue = D Belial
E Cesium #>> P Red = E Deuterium
E Cesium #>> P Green = E Cesium
E Cesium #>> P Blue = P Blue

-- Element + Letter
E Deuterium #>> L Lambda = P Red
E Deuterium #>> L Mu = L Lambda
E Deuterium #>> L Omega = E Deuterium
E Erbium #>> L Lambda = E Erbium
E Erbium #>> L Mu = E Deuterium
E Erbium #>> L Omega = P Red
E Cesium #>> L Lambda = G Hypnos
E Cesium #>> L Mu = D Belial
E Cesium #>> L Omega = L Omega

-- Element + Element
E Deuterium #>> E Deuterium = E Deuterium
E Deuterium #>> E Erbium = P Red
E Deuterium #>> E Cesium = G Nyx
E Erbium #>> E Deuterium = L Omega
E Erbium #>> E Erbium = P Blue
E Erbium #>> E Cesium = E Deuterium
E Cesium #>> E Deuterium = L Mu
E Cesium #>> E Erbium = L Mu
E Cesium #>> E Cesium = E Erbium

-- Element + God
E Deuterium #>> G Nyx = L Omega
E Deuterium #>> G Hypnos = L Lambda
E Deuterium #>> G Thanatos = T Piston
E Erbium #>> G Nyx = G Hypnos
E Erbium #>> G Hypnos = G Hypnos
E Erbium #>> G Thanatos = P Red
E Cesium #>> G Nyx = G Hypnos
E Cesium #>> G Hypnos = P Red
E Cesium #>> G Thanatos = P Red

-- Element + Demon
E Deuterium #>> D Mammon = E Erbium
E Deuterium #>> D Asmodeus = E Deuterium
E Deuterium #>> D Belial = D Belial
E Erbium #>> D Mammon = P Red
E Erbium #>> D Asmodeus = E Erbium
E Erbium #>> D Belial = E Deuterium
E Cesium #>> D Mammon = D Asmodeus
E Cesium #>> D Asmodeus = P Green
E Cesium #>> D Belial = E Deuterium

-- Element + Tech
E Deuterium #>> T Piston = T Transistor
E Deuterium #>> T Transistor = D Belial
E Deuterium #>> T Gear = P Red
E Erbium #>> T Piston = E Erbium
E Erbium #>> T Transistor = G Nyx
E Erbium #>> T Gear = L Mu
E Cesium #>> T Piston = E Erbium
E Cesium #>> T Transistor = G Hypnos
E Cesium #>> T Gear = P Red

-- God + White
G Nyx #>> White = L Lambda
G Hypnos #>> White = L Omega
G Thanatos #>> White = E Cesium

-- God + Black
G Nyx #>> Black = Black
G Hypnos #>> Black = D Mammon
G Thanatos #>> Black = P Green

-- God + Primary
G Nyx #>> P Red = E Deuterium
G Nyx #>> P Green = P Blue
G Nyx #>> P Blue = P Red
G Hypnos #>> P Red = P Red
G Hypnos #>> P Green = E Erbium
G Hypnos #>> P Blue = P Red
G Thanatos #>> P Red = P Red
G Thanatos #>> P Green = D Mammon
G Thanatos #>> P Blue = G Thanatos

-- God + Letter
G Nyx #>> L Lambda = Black
G Nyx #>> L Mu = E Cesium
G Nyx #>> L Omega = P Green
G Hypnos #>> L Lambda = T Piston
G Hypnos #>> L Mu = G Thanatos
G Hypnos #>> L Omega = L Omega
G Thanatos #>> L Lambda = P Green
G Thanatos #>> L Mu = P Red
G Thanatos #>> L Omega = P Blue

-- God + Element
G Nyx #>> E Deuterium = E Cesium
G Nyx #>> E Erbium = G Nyx
G Nyx #>> E Cesium = G Thanatos
G Hypnos #>> E Deuterium = G Nyx
G Hypnos #>> E Erbium = P Red
G Hypnos #>> E Cesium = L Lambda
G Thanatos #>> E Deuterium = E Deuterium
G Thanatos #>> E Erbium = L Mu
G Thanatos #>> E Cesium = E Cesium

-- God + God
G Nyx #>> G Nyx = E Deuterium
G Nyx #>> G Hypnos = P Green
G Nyx #>> G Thanatos = G Nyx
G Hypnos #>> G Nyx = T Gear
G Hypnos #>> G Hypnos = L Lambda
G Hypnos #>> G Thanatos = G Nyx
G Thanatos #>> G Nyx = G Thanatos
G Thanatos #>> G Hypnos = E Erbium
G Thanatos #>> G Thanatos = P Green

-- God + Demon
G Nyx #>> D Mammon = P Red
G Nyx #>> D Asmodeus = Black
G Nyx #>> D Belial = P Red
G Hypnos #>> D Mammon = G Hypnos
G Hypnos #>> D Asmodeus = P Red
G Hypnos #>> D Belial = P Red
G Thanatos #>> D Mammon = L Lambda
G Thanatos #>> D Asmodeus = L Lambda
G Thanatos #>> D Belial = L Lambda

-- God + Tech
G Nyx #>> T Piston = E Deuterium
G Nyx #>> T Transistor = D Asmodeus
G Nyx #>> T Gear = P Blue
G Hypnos #>> T Piston = E Deuterium
G Hypnos #>> T Transistor = E Erbium
G Hypnos #>> T Gear = E Erbium
G Thanatos #>> T Piston = E Erbium
G Thanatos #>> T Transistor = P Red
G Thanatos #>> T Gear = D Asmodeus

-- Demon + White
D Mammon #>> White = P Red
D Asmodeus #>> White = P Blue
D Belial #>> White = E Deuterium

-- Demon + Black
D Mammon #>> Black = G Nyx
D Asmodeus #>> Black = P Red
D Belial #>> Black = G Nyx

-- Demon + Primary
D Mammon #>> P Red = T Piston
D Mammon #>> P Green = P Red
D Mammon #>> P Blue = L Lambda
D Asmodeus #>> P Red = D Belial
D Asmodeus #>> P Green = P Blue
D Asmodeus #>> P Blue = G Nyx
D Belial #>> P Red = P Red
D Belial #>> P Green = G Hypnos
D Belial #>> P Blue = L Omega

-- Demon + Letter
D Mammon #>> L Lambda = G Nyx
D Mammon #>> L Mu = L Omega
D Mammon #>> L Omega = L Lambda
D Asmodeus #>> L Lambda = E Deuterium
D Asmodeus #>> L Mu = P Red
D Asmodeus #>> L Omega = E Deuterium
D Belial #>> L Lambda = P Green
D Belial #>> L Mu = L Lambda
D Belial #>> L Omega = E Erbium

-- Demon + Element
D Mammon #>> E Deuterium = Black
D Mammon #>> E Erbium = P Red
D Mammon #>> E Cesium = P Green
D Asmodeus #>> E Deuterium = P Red
D Asmodeus #>> E Erbium = E Cesium
D Asmodeus #>> E Cesium = P Red
D Belial #>> E Deuterium = L Lambda
D Belial #>> E Erbium = L Lambda
D Belial #>> E Cesium = G Thanatos

-- Demon + God
D Mammon #>> G Nyx = D Asmodeus
D Mammon #>> G Hypnos = P Red
D Mammon #>> G Thanatos = P Red
D Asmodeus #>> G Nyx = L Omega
D Asmodeus #>> G Hypnos = D Asmodeus
D Asmodeus #>> G Thanatos = G Nyx
D Belial #>> G Nyx = L Lambda
D Belial #>> G Hypnos = P Blue
D Belial #>> G Thanatos = P Red

-- Demon + Demon
D Mammon #>> D Mammon = P Green
D Mammon #>> D Asmodeus = D Asmodeus
D Mammon #>> D Belial = Black
D Asmodeus #>> D Mammon = E Erbium
D Asmodeus #>> D Asmodeus = P Red
D Asmodeus #>> D Belial = Black
D Belial #>> D Mammon = D Mammon
D Belial #>> D Asmodeus = E Erbium
D Belial #>> D Belial = G Hypnos

-- Demon + Tech
D Mammon #>> T Piston = P Red
D Mammon #>> T Transistor = D Mammon
D Mammon #>> T Gear = P Blue
D Asmodeus #>> T Piston = G Nyx
D Asmodeus #>> T Transistor = L Mu
D Asmodeus #>> T Gear = L Lambda
D Belial #>> T Piston = L Lambda
D Belial #>> T Transistor = D Mammon
D Belial #>> T Gear = E Deuterium

-- Tech + White
T Piston #>> White = E Cesium
T Transistor #>> White = P Blue
T Gear #>> White = G Nyx

-- Tech + Black
T Piston #>> Black = P Red
T Transistor #>> Black = P Red
T Gear #>> Black = P Red

-- Tech + Primary
T Piston #>> P Red = L Lambda
T Piston #>> P Green = E Deuterium
T Piston #>> P Blue = P Red
T Transistor #>> P Red = L Lambda
T Transistor #>> P Green = E Deuterium
T Transistor #>> P Blue = G Nyx
T Gear #>> P Red = P Green
T Gear #>> P Green = G Nyx
T Gear #>> P Blue = L Lambda

-- Tech + Letter
T Piston #>> L Lambda = E Erbium
T Piston #>> L Mu = D Asmodeus
T Piston #>> L Omega = G Hypnos
T Transistor #>> L Lambda = G Nyx
T Transistor #>> L Mu = L Lambda
T Transistor #>> L Omega = L Lambda
T Gear #>> L Lambda = L Lambda
T Gear #>> L Mu = P Blue
T Gear #>> L Omega = L Lambda

-- Tech + Element
T Piston #>> E Deuterium = G Nyx
T Piston #>> E Erbium = T Gear
T Piston #>> E Cesium = G Thanatos
T Transistor #>> E Deuterium = G Nyx
T Transistor #>> E Erbium = E Erbium
T Transistor #>> E Cesium = P Blue
T Gear #>> E Deuterium = G Nyx
T Gear #>> E Erbium = P Red
T Gear #>> E Cesium = P Red

-- Tech + God
T Piston #>> G Nyx = L Omega
T Piston #>> G Hypnos = E Deuterium
T Piston #>> G Thanatos = White
T Transistor #>> G Nyx = E Erbium
T Transistor #>> G Hypnos = E Erbium
T Transistor #>> G Thanatos = White
T Gear #>> G Nyx = Black
T Gear #>> G Hypnos = L Omega
T Gear #>> G Thanatos = E Cesium

-- Tech + Demon
T Piston #>> D Mammon = L Lambda
T Piston #>> D Asmodeus = P Red
T Piston #>> D Belial = G Nyx
T Transistor #>> D Mammon = E Deuterium
T Transistor #>> D Asmodeus = D Asmodeus
T Transistor #>> D Belial = P Blue
T Gear #>> D Mammon = D Asmodeus
T Gear #>> D Asmodeus = D Belial
T Gear #>> D Belial = T Transistor

-- Tech + Tech
T Piston #>> T Piston = L Lambda
T Piston #>> T Transistor = D Asmodeus
T Piston #>> T Gear = P Red
T Transistor #>> T Piston = E Deuterium
T Transistor #>> T Transistor = P Green
T Transistor #>> T Gear = G Nyx
T Gear #>> T Piston = E Erbium
T Gear #>> T Transistor = L Mu
T Gear #>> T Gear = G Hypnos


-- Finally done!

instance Random Primary where
    randomR (a,z) g = (toEnum (retval `mod` 3 ),g')
        where (retval,g') = randomR (fromEnum a ,fromEnum z ) g
    random = randomR (minBound,maxBound)
instance Random Letter where
    randomR (a,z) g = (toEnum (retval `mod` 3 ),g')
        where (retval,g') = randomR (fromEnum a ,fromEnum z ) g
    random = randomR (minBound,maxBound)
instance Random God where
    randomR (a,z) g = (toEnum (retval `mod` 3 ),g')
        where (retval,g') = randomR (fromEnum a ,fromEnum z ) g
    random = randomR (minBound,maxBound)
instance Random Demon where
    randomR (a,z) g = (toEnum (retval `mod` 3 ),g')
        where (retval,g') = randomR (fromEnum a ,fromEnum z ) g
    random = randomR (minBound,maxBound)
instance Random Element where
    randomR (a,z) g = (toEnum (retval `mod` 3 ),g')
        where (retval,g') = randomR (fromEnum a ,fromEnum z ) g
    random = randomR (minBound,maxBound)
instance Random Tech where
    randomR (a,z) g = (toEnum (retval `mod` 3 ),g')
        where (retval,g') = randomR (fromEnum a ,fromEnum z ) g
    random = randomR (minBound,maxBound)
instance Random Orb where
    randomR (a,z) g = (toEnum (retval `mod` 21 ),g')
        where (retval,g') = randomR (fromEnum a ,fromEnum z ) g
    random = randomR (minBound,maxBound)

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
      combined = orb xx #>> orb yy