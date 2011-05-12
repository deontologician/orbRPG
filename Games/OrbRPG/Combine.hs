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

data Primary = Red | Green  | Blue 
               deriving (Ord, Eq, Read, Show, Enum, Bounded)

data Letter = Lambda | Mu | Omega 
            deriving (Ord, Eq, Enum, Bounded)

data God = Nyx | Hypnos | Thanatos
         deriving (Ord, Eq, Read, Show, Enum, Bounded)

data Demon = Mammon | Asmodeus | Belial
           deriving (Ord, Eq, Read, Show, Enum, Bounded)

data Element = Deuterium | Erbium | Gold
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
-- Combinations that result in Null
D _ #>> P _ = Null
G _ #>> L _ = Null
L _ #>> G _ = Null
P _ #>> D _ = Null

-- Black combinations
Black #>> Black = Black
Black #>> White = Null
Black #>> P _ = T Piston
P _   #>> Black = L Lambda
Black #>> L _ = P Red
L _   #>> Black = G Nyx
Black #>> G _ = L Lambda
G _   #>> Black = D Mammon
Black #>> D _ = G Nyx
D _   #>> Black = E Deuterium
Black #>> E _ = D Mammon
E _   #>> Black = T Piston
Black #>> T _ = E Deuterium
T _   #>> Black = P Red
-- Combinations that result in Black
G _ #>> D _ = Black
L _ #>> E _ = Black
P _ #>> T _ = Black

-- White combinations
White #>> White = White
White #>> Black = Null
White #>> P _ = L Lambda
P _   #>> White = T Piston
White #>> L _ = G Nyx
L _   #>> White = P Red
White #>> G _ = D Mammon
G _   #>> White = L Lambda
White #>> D _ = E Deuterium
D _   #>> White = G Nyx
White #>> E _ = T Piston
E _   #>> White = D Mammon
White #>> T _ = P Red
T _   #>> White = E Deuterium
-- Combinations that result in White
T _   #>> P _ = White
E _   #>> L _ = White
D _   #>> G _ = White

-- Primary + Primary = Primary
P Red #>> P Red = P Blue
P Red #>> P Green = P Red
P Red #>> P Blue = P Red
P Green #>> P Red = P Red
P Green #>> P Green = P Blue
P Green #>> P Blue = P Green
P Blue #>> P Red = P Green
P Blue #>> P Green = P Green
P Blue #>> P Blue = P Green

-- Letter + Letter = Letter
L Lambda #>> L Lambda = L Omega
L Lambda #>> L Mu = L Omega
L Lambda #>> L Omega = L Mu
L Mu #>> L Lambda = L Mu
L Mu #>> L Mu = L Omega
L Mu #>> L Omega = L Omega
L Omega #>> L Lambda = L Omega
L Omega #>> L Mu = L Lambda
L Omega #>> L Omega = L Omega

-- God + God = God
G Nyx #>> G Nyx = G Thanatos
G Nyx #>> G Hypnos = G Thanatos
G Nyx #>> G Thanatos = G Nyx
G Hypnos #>> G Nyx = G Nyx
G Hypnos #>> G Hypnos = G Nyx
G Hypnos #>> G Thanatos = G Nyx
G Thanatos #>> G Nyx = G Hypnos
G Thanatos #>> G Hypnos = G Thanatos
G Thanatos #>> G Thanatos = G Thanatos

-- Demon + Demon = Demon
D Mammon #>> D Mammon = D Asmodeus
D Mammon #>> D Asmodeus = D Asmodeus
D Mammon #>> D Belial = D Belial
D Asmodeus #>> D Mammon = D Asmodeus
D Asmodeus #>> D Asmodeus = D Belial
D Asmodeus #>> D Belial = D Belial
D Belial #>> D Mammon = D Belial
D Belial #>> D Asmodeus = D Belial
D Belial #>> D Belial = D Belial

-- Element + Element = Element
E Deuterium #>> E Deuterium = E Deuterium
E Deuterium #>> E Erbium = E Erbium
E Deuterium #>> E Gold = E Gold
E Erbium #>> E Deuterium = E Deuterium
E Erbium #>> E Erbium = E Erbium
E Erbium #>> E Gold = E Erbium
E Gold #>> E Deuterium = E Erbium
E Gold #>> E Erbium = E Gold
E Gold #>> E Gold = E Deuterium

-- Tech + Tech = Tech
T Piston #>> T Piston = T Transistor
T Piston #>> T Transistor = T Gear
T Piston #>> T Gear = T Gear
T Transistor #>> T Piston = T Piston
T Transistor #>> T Transistor = T Transistor
T Transistor #>> T Gear = T Gear
T Gear #>> T Piston = T Piston
T Gear #>> T Transistor = T Gear
T Gear #>> T Gear = T Transistor

-- Primary + Letter = Letter
P Red #>> L Lambda = L Mu
P Red #>> L Mu = L Lambda
P Red #>> L Omega = L Mu
P Green #>> L Lambda = L Mu
P Green #>> L Mu = L Mu
P Green #>> L Omega = L Mu
P Blue #>> L Lambda = L Mu
P Blue #>> L Mu = L Omega
P Blue #>> L Omega = L Mu

-- Primary + God = Letter
P Red #>> G Nyx = L Lambda
P Red #>> G Hypnos = L Mu
P Red #>> G Thanatos = L Lambda
P Green #>> G Nyx = L Mu
P Green #>> G Hypnos = L Omega
P Green #>> G Thanatos = L Lambda
P Blue #>> G Nyx = L Lambda
P Blue #>> G Hypnos = L Mu
P Blue #>> G Thanatos = L Omega

-- Primary + Element = Element
P Red #>> E Deuterium = E Erbium
P Red #>> E Erbium = E Deuterium
P Red #>> E Gold = E Gold
P Green #>> E Deuterium = E Gold
P Green #>> E Erbium = E Deuterium
P Green #>> E Gold = E Erbium
P Blue #>> E Deuterium = E Deuterium
P Blue #>> E Erbium = E Erbium
P Blue #>> E Gold = E Deuterium

-- Letter + Primary = Letter
L Lambda #>> P Red = L Lambda
L Lambda #>> P Green = L Lambda
L Lambda #>> P Blue = L Omega
L Mu #>> P Red = L Lambda
L Mu #>> P Green = L Mu
L Mu #>> P Blue = L Lambda
L Omega #>> P Red = L Omega
L Omega #>> P Green = L Mu
L Omega #>> P Blue = L Omega

-- Letter + Demon = God
L Lambda #>> D Mammon = G Thanatos
L Lambda #>> D Asmodeus = G Nyx
L Lambda #>> D Belial = G Nyx
L Mu #>> D Mammon = G Nyx
L Mu #>> D Asmodeus = G Nyx
L Mu #>> D Belial = G Nyx
L Omega #>> D Mammon = G Nyx
L Omega #>> D Asmodeus = G Thanatos
L Omega #>> D Belial = G Nyx

-- Letter + Tech = God
L Lambda #>> T Piston = G Nyx
L Lambda #>> T Transistor = G Nyx
L Lambda #>> T Gear = G Nyx
L Mu #>> T Piston = G Hypnos
L Mu #>> T Transistor = G Nyx
L Mu #>> T Gear = G Hypnos
L Omega #>> T Piston = G Nyx
L Omega #>> T Transistor = G Thanatos
L Omega #>> T Gear = G Thanatos

-- God + Primary = Primary
G Nyx #>> P Red = P Red
G Nyx #>> P Green = P Red
G Nyx #>> P Blue = P Red
G Hypnos #>> P Red = P Green
G Hypnos #>> P Green = P Green
G Hypnos #>> P Blue = P Blue
G Thanatos #>> P Red = P Green
G Thanatos #>> P Green = P Red
G Thanatos #>> P Blue = P Green

-- God + Element = Letter
G Nyx #>> E Deuterium = L Omega
G Nyx #>> E Erbium = L Lambda
G Nyx #>> E Gold = L Omega
G Hypnos #>> E Deuterium = L Omega
G Hypnos #>> E Erbium = L Lambda
G Hypnos #>> E Gold = L Mu
G Thanatos #>> E Deuterium = L Omega
G Thanatos #>> E Erbium = L Mu
G Thanatos #>> E Gold = L Lambda

-- God + Tech = Letter
G Nyx #>> T Piston = L Omega
G Nyx #>> T Transistor = L Omega
G Nyx #>> T Gear = L Lambda
G Hypnos #>> T Piston = L Mu
G Hypnos #>> T Transistor = L Mu
G Hypnos #>> T Gear = L Lambda
G Thanatos #>> T Piston = L Mu
G Thanatos #>> T Transistor = L Mu
G Thanatos #>> T Gear = L Omega
-- Demon + Letter #>>=Primary
D Mammon #>> L Lambda = P Red
D Mammon #>> L Mu = P Blue
D Mammon #>> L Omega = P Red
D Asmodeus #>> L Lambda = P Red
D Asmodeus #>> L Mu = P Green
D Asmodeus #>> L Omega = P Green
D Belial #>> L Lambda = P Red
D Belial #>> L Mu = P Red
D Belial #>> L Omega = P Red

-- Demon + Element = Element
D Mammon #>> E Deuterium = E Erbium
D Mammon #>> E Erbium = E Gold
D Mammon #>> E Gold = E Erbium
D Asmodeus #>> E Deuterium = E Deuterium
D Asmodeus #>> E Erbium = E Deuterium
D Asmodeus #>> E Gold = E Erbium
D Belial #>> E Deuterium = E Deuterium
D Belial #>> E Erbium = E Deuterium
D Belial #>> E Gold = E Erbium

-- Demon + Tech = God
D Mammon #>> T Piston = G Thanatos
D Mammon #>> T Transistor = G Hypnos
D Mammon #>> T Gear = G Nyx
D Asmodeus #>> T Piston = G Thanatos
D Asmodeus #>> T Transistor = G Thanatos
D Asmodeus #>> T Gear = G Nyx
D Belial #>> T Piston = G Nyx
D Belial #>> T Transistor = G Nyx
D Belial #>> T Gear = G Nyx

-- Element + Primary = God
E Deuterium #>> P Red = G Hypnos
E Deuterium #>> P Green = G Hypnos
E Deuterium #>> P Blue = G Nyx
E Erbium #>> P Red = G Nyx
E Erbium #>> P Green = G Hypnos
E Erbium #>> P Blue = G Hypnos
E Gold #>> P Red = G Nyx
E Gold #>> P Green = G Thanatos
E Gold #>> P Blue = G Thanatos

-- Element + God = Primary
E Deuterium #>> G Nyx = P Red
E Deuterium #>> G Hypnos = P Green
E Deuterium #>> G Thanatos = P Red
E Erbium #>> G Nyx = P Blue
E Erbium #>> G Hypnos = P Blue
E Erbium #>> G Thanatos = P Green
E Gold #>> G Nyx = P Red
E Gold #>> G Hypnos = P Red
E Gold #>> G Thanatos = P Blue

-- Element + Demon = Element
E Deuterium #>> D Mammon = E Gold
E Deuterium #>> D Asmodeus = E Gold
E Deuterium #>> D Belial = E Erbium
E Erbium #>> D Mammon = E Gold
E Erbium #>> D Asmodeus = E Erbium
E Erbium #>> D Belial = E Deuterium
E Gold #>> D Mammon = E Gold
E Gold #>> D Asmodeus = E Erbium
E Gold #>> D Belial = E Deuterium

-- Element + Tech = Tech
E Deuterium #>> T Piston = T Piston
E Deuterium #>> T Transistor = T Transistor
E Deuterium #>> T Gear = T Transistor
E Erbium #>> T Piston = T Piston
E Erbium #>> T Transistor = T Piston
E Erbium #>> T Gear = T Transistor
E Gold #>> T Piston = T Transistor
E Gold #>> T Transistor = T Gear
E Gold #>> T Gear = T Transistor

-- Tech + Letter = God
T Piston #>> L Lambda = G Nyx
T Piston #>> L Mu = G Nyx
T Piston #>> L Omega = G Hypnos
T Transistor #>> L Lambda = G Thanatos
T Transistor #>> L Mu = G Thanatos
T Transistor #>> L Omega = G Nyx
T Gear #>> L Lambda = G Thanatos
T Gear #>> L Mu = G Thanatos
T Gear #>> L Omega = G Thanatos

-- Tech + God = Letter
T Piston #>> G Nyx = L Mu
T Piston #>> G Hypnos = L Omega
T Piston #>> G Thanatos = L Omega
T Transistor #>> G Nyx = L Omega
T Transistor #>> G Hypnos = L Omega
T Transistor #>> G Thanatos = L Lambda
T Gear #>> G Nyx = L Mu
T Gear #>> G Hypnos = L Omega
T Gear #>> G Thanatos = L Mu
-- Tech + Demon #>>=Primary
T Piston #>> D Mammon = P Green
T Piston #>> D Asmodeus = P Blue
T Piston #>> D Belial = P Red
T Transistor #>> D Mammon = P Green
T Transistor #>> D Asmodeus = P Red
T Transistor #>> D Belial = P Red
T Gear #>> D Mammon = P Green
T Gear #>> D Asmodeus = P Red
T Gear #>> D Belial = P Green

-- Tech + Element = Tech
T Piston #>> E Deuterium = T Transistor
T Piston #>> E Erbium = T Piston
T Piston #>> E Gold = T Piston
T Transistor #>> E Deuterium = T Gear
T Transistor #>> E Erbium = T Gear
T Transistor #>> E Gold = T Gear
T Gear #>> E Deuterium = T Piston
T Gear #>> E Erbium = T Piston
T Gear #>> E Gold = T Piston

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

data Ctree a = CNode {sub :: Orb
                     ,attr :: a
                     ,cLeft :: Ctree a
                     ,cRight :: Ctree a
                     } 
             | CLeaf {sub :: Orb
                     ,attr :: a}


combinate :: (Orb -> a -> a -> a) -> Ctree a -> Ctree a -> Ctree a
combinate f xx yy = 
    CNode{sub = combined
         ,attr = f combined (attr xx) (attr yy)
         ,cLeft = xx
         ,cRight = yy}
    where 
      combined :: Orb
      combined = sub xx #>> sub yy

