{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}
module Combine where

import System.Random

data Substance = Null | Black | White | P Primary | L Letter | G God 
               | D Demon | E Element | T Tech
                 deriving (Ord, Eq, Read, Show)

instance Bounded Substance where
    minBound = Null
    maxBound = T Gear

instance Enum Substance where
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
             | otherwise = error "Int out of range for Substance Enum"

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
combine :: Substance -> Substance -> Substance
-- Null Combinations
combine _ Null = Null
combine Null Black = White
combine Null White = Black
combine Null _     = Null
-- Combinations that result in Null
combine (D _) (P _) = Null
combine (G _) (L _) = Null
combine (L _) (G _) = Null
combine (P _) (D _) = Null

-- Black combinations
combine Black Black = Black
combine Black White = Null
combine Black (P _) = T Piston
combine (P _) Black = L Lambda
combine Black (L _) = P Red
combine (L _) Black = G Nyx
combine Black (G _) = L Lambda
combine (G _) Black = D Mammon
combine Black (D _) = G Nyx
combine (D _) Black = E Deuterium
combine Black (E _) = D Mammon
combine (E _) Black = T Piston
combine Black (T _) = E Deuterium
combine (T _) Black = P Red
-- Combinations that result in Black
combine (G _) (D _) = Black
combine (L _) (E _) = Black
combine (P _) (T _) = Black

-- White combinations
combine White White = White
combine White Black = Null
combine White (P _) = L Lambda
combine (P _) White = T Piston
combine White (L _) = G Nyx
combine (L _) White = P Red
combine White (G _) = D Mammon
combine (G _) White = L Lambda
combine White (D _) = E Deuterium
combine (D _) White = G Nyx
combine White (E _) = T Piston
combine (E _) White = D Mammon
combine White (T _) = P Red
combine (T _) White = E Deuterium
-- Combinations that result in White
combine (T _) (P _) = White
combine (E _) (L _) = White
combine (D _) (G _) = White

-- Primary + Primary = Primary
combine (P Red) (P Red) = P Blue
combine (P Red) (P Green) = P Red
combine (P Red) (P Blue) = P Red
combine (P Green) (P Red) = P Red
combine (P Green) (P Green) = P Blue
combine (P Green) (P Blue) = P Green
combine (P Blue) (P Red) = P Green
combine (P Blue) (P Green) = P Green
combine (P Blue) (P Blue) = P Green

-- Letter + Letter = Letter
combine (L Lambda) (L Lambda) = L Omega
combine (L Lambda) (L Mu) = L Omega
combine (L Lambda) (L Omega) = L Mu
combine (L Mu) (L Lambda) = L Mu
combine (L Mu) (L Mu) = L Omega
combine (L Mu) (L Omega) = L Omega
combine (L Omega) (L Lambda) = L Omega
combine (L Omega) (L Mu) = L Lambda
combine (L Omega) (L Omega) = L Omega

-- God + God = God
combine (G Nyx) (G Nyx) = G Thanatos
combine (G Nyx) (G Hypnos) = G Thanatos
combine (G Nyx) (G Thanatos) = G Nyx
combine (G Hypnos) (G Nyx) = G Nyx
combine (G Hypnos) (G Hypnos) = G Nyx
combine (G Hypnos) (G Thanatos) = G Nyx
combine (G Thanatos) (G Nyx) = G Hypnos
combine (G Thanatos) (G Hypnos) = G Thanatos
combine (G Thanatos) (G Thanatos) = G Thanatos

-- Demon + Demon = Demon
combine (D Mammon) (D Mammon) = D Asmodeus
combine (D Mammon) (D Asmodeus) = D Asmodeus
combine (D Mammon) (D Belial) = D Belial
combine (D Asmodeus) (D Mammon) = D Asmodeus
combine (D Asmodeus) (D Asmodeus) = D Belial
combine (D Asmodeus) (D Belial) = D Belial
combine (D Belial) (D Mammon) = D Belial
combine (D Belial) (D Asmodeus) = D Belial
combine (D Belial) (D Belial) = D Belial

-- Element + Element = Element
combine (E Deuterium) (E Deuterium) = E Deuterium
combine (E Deuterium) (E Erbium) = E Erbium
combine (E Deuterium) (E Gold) = E Gold
combine (E Erbium) (E Deuterium) = E Deuterium
combine (E Erbium) (E Erbium) = E Erbium
combine (E Erbium) (E Gold) = E Erbium
combine (E Gold) (E Deuterium) = E Erbium
combine (E Gold) (E Erbium) = E Gold
combine (E Gold) (E Gold) = E Deuterium

-- Tech + Tech = Tech
combine (T Piston) (T Piston) = T Transistor
combine (T Piston) (T Transistor) = T Gear
combine (T Piston) (T Gear) = T Gear
combine (T Transistor) (T Piston) = T Piston
combine (T Transistor) (T Transistor) = T Transistor
combine (T Transistor) (T Gear) = T Gear
combine (T Gear) (T Piston) = T Piston
combine (T Gear) (T Transistor) = T Gear
combine (T Gear) (T Gear) = T Transistor

-- Primary + Letter = Letter
combine (P Red) (L Lambda) = L Mu
combine (P Red) (L Mu) = L Lambda
combine (P Red) (L Omega) = L Mu
combine (P Green) (L Lambda) = L Mu
combine (P Green) (L Mu) = L Mu
combine (P Green) (L Omega) = L Mu
combine (P Blue) (L Lambda) = L Mu
combine (P Blue) (L Mu) = L Omega
combine (P Blue) (L Omega) = L Mu

-- Primary + God = Letter
combine (P Red) (G Nyx) = L Lambda
combine (P Red) (G Hypnos) = L Mu
combine (P Red) (G Thanatos) = L Lambda
combine (P Green) (G Nyx) = L Mu
combine (P Green) (G Hypnos) = L Omega
combine (P Green) (G Thanatos) = L Lambda
combine (P Blue) (G Nyx) = L Lambda
combine (P Blue) (G Hypnos) = L Mu
combine (P Blue) (G Thanatos) = L Omega

-- Primary + Element = Element
combine (P Red) (E Deuterium) = E Erbium
combine (P Red) (E Erbium) = E Deuterium
combine (P Red) (E Gold) = E Gold
combine (P Green) (E Deuterium) = E Gold
combine (P Green) (E Erbium) = E Deuterium
combine (P Green) (E Gold) = E Erbium
combine (P Blue) (E Deuterium) = E Deuterium
combine (P Blue) (E Erbium) = E Erbium
combine (P Blue) (E Gold) = E Deuterium

-- Letter + Primary = Letter
combine (L Lambda) (P Red) = L Lambda
combine (L Lambda) (P Green) = L Lambda
combine (L Lambda) (P Blue) = L Omega
combine (L Mu) (P Red) = L Lambda
combine (L Mu) (P Green) = L Mu
combine (L Mu) (P Blue) = L Lambda
combine (L Omega) (P Red) = L Omega
combine (L Omega) (P Green) = L Mu
combine (L Omega) (P Blue) = L Omega

-- Letter + Demon = God
combine (L Lambda) (D Mammon) = G Thanatos
combine (L Lambda) (D Asmodeus) = G Nyx
combine (L Lambda) (D Belial) = G Nyx
combine (L Mu) (D Mammon) = G Nyx
combine (L Mu) (D Asmodeus) = G Nyx
combine (L Mu) (D Belial) = G Nyx
combine (L Omega) (D Mammon) = G Nyx
combine (L Omega) (D Asmodeus) = G Thanatos
combine (L Omega) (D Belial) = G Nyx

-- Letter + Tech = God
combine (L Lambda) (T Piston) = G Nyx
combine (L Lambda) (T Transistor) = G Nyx
combine (L Lambda) (T Gear) = G Nyx
combine (L Mu) (T Piston) = G Hypnos
combine (L Mu) (T Transistor) = G Nyx
combine (L Mu) (T Gear) = G Hypnos
combine (L Omega) (T Piston) = G Nyx
combine (L Omega) (T Transistor) = G Thanatos
combine (L Omega) (T Gear) = G Thanatos

-- God + Primary = Primary
combine (G Nyx) (P Red) = P Red
combine (G Nyx) (P Green) = P Red
combine (G Nyx) (P Blue) = P Red
combine (G Hypnos) (P Red) = P Green
combine (G Hypnos) (P Green) = P Green
combine (G Hypnos) (P Blue) = P Blue
combine (G Thanatos) (P Red) = P Green
combine (G Thanatos) (P Green) = P Red
combine (G Thanatos) (P Blue) = P Green

-- God + Element = Letter
combine (G Nyx) (E Deuterium) = L Omega
combine (G Nyx) (E Erbium) = L Lambda
combine (G Nyx) (E Gold) = L Omega
combine (G Hypnos) (E Deuterium) = L Omega
combine (G Hypnos) (E Erbium) = L Lambda
combine (G Hypnos) (E Gold) = L Mu
combine (G Thanatos) (E Deuterium) = L Omega
combine (G Thanatos) (E Erbium) = L Mu
combine (G Thanatos) (E Gold) = L Lambda

-- God + Tech = Letter
combine (G Nyx) (T Piston) = L Omega
combine (G Nyx) (T Transistor) = L Omega
combine (G Nyx) (T Gear) = L Lambda
combine (G Hypnos) (T Piston) = L Mu
combine (G Hypnos) (T Transistor) = L Mu
combine (G Hypnos) (T Gear) = L Lambda
combine (G Thanatos) (T Piston) = L Mu
combine (G Thanatos) (T Transistor) = L Mu
combine (G Thanatos) (T Gear) = L Omega

-- Demon + Letter = Primary
combine (D Mammon) (L Lambda) = P Red
combine (D Mammon) (L Mu) = P Blue
combine (D Mammon) (L Omega) = P Red
combine (D Asmodeus) (L Lambda) = P Red
combine (D Asmodeus) (L Mu) = P Green
combine (D Asmodeus) (L Omega) = P Green
combine (D Belial) (L Lambda) = P Red
combine (D Belial) (L Mu) = P Red
combine (D Belial) (L Omega) = P Red

-- Demon + Element = Element
combine (D Mammon) (E Deuterium) = E Erbium
combine (D Mammon) (E Erbium) = E Gold
combine (D Mammon) (E Gold) = E Erbium
combine (D Asmodeus) (E Deuterium) = E Deuterium
combine (D Asmodeus) (E Erbium) = E Deuterium
combine (D Asmodeus) (E Gold) = E Erbium
combine (D Belial) (E Deuterium) = E Deuterium
combine (D Belial) (E Erbium) = E Deuterium
combine (D Belial) (E Gold) = E Erbium

-- Demon + Tech = God
combine (D Mammon) (T Piston) = G Thanatos
combine (D Mammon) (T Transistor) = G Hypnos
combine (D Mammon) (T Gear) = G Nyx
combine (D Asmodeus) (T Piston) = G Thanatos
combine (D Asmodeus) (T Transistor) = G Thanatos
combine (D Asmodeus) (T Gear) = G Nyx
combine (D Belial) (T Piston) = G Nyx
combine (D Belial) (T Transistor) = G Nyx
combine (D Belial) (T Gear) = G Nyx

-- Element + Primary = God
combine (E Deuterium) (P Red) = G Hypnos
combine (E Deuterium) (P Green) = G Hypnos
combine (E Deuterium) (P Blue) = G Nyx
combine (E Erbium) (P Red) = G Nyx
combine (E Erbium) (P Green) = G Hypnos
combine (E Erbium) (P Blue) = G Hypnos
combine (E Gold) (P Red) = G Nyx
combine (E Gold) (P Green) = G Thanatos
combine (E Gold) (P Blue) = G Thanatos

-- Element + God = Primary
combine (E Deuterium) (G Nyx) = P Red
combine (E Deuterium) (G Hypnos) = P Green
combine (E Deuterium) (G Thanatos) = P Red
combine (E Erbium) (G Nyx) = P Blue
combine (E Erbium) (G Hypnos) = P Blue
combine (E Erbium) (G Thanatos) = P Green
combine (E Gold) (G Nyx) = P Red
combine (E Gold) (G Hypnos) = P Red
combine (E Gold) (G Thanatos) = P Blue

-- Element + Demon = Element
combine (E Deuterium) (D Mammon) = E Gold
combine (E Deuterium) (D Asmodeus) = E Gold
combine (E Deuterium) (D Belial) = E Erbium
combine (E Erbium) (D Mammon) = E Gold
combine (E Erbium) (D Asmodeus) = E Erbium
combine (E Erbium) (D Belial) = E Deuterium
combine (E Gold) (D Mammon) = E Gold
combine (E Gold) (D Asmodeus) = E Erbium
combine (E Gold) (D Belial) = E Deuterium

-- Element + Tech = Tech
combine (E Deuterium) (T Piston) = T Piston
combine (E Deuterium) (T Transistor) = T Transistor
combine (E Deuterium) (T Gear) = T Transistor
combine (E Erbium) (T Piston) = T Piston
combine (E Erbium) (T Transistor) = T Piston
combine (E Erbium) (T Gear) = T Transistor
combine (E Gold) (T Piston) = T Transistor
combine (E Gold) (T Transistor) = T Gear
combine (E Gold) (T Gear) = T Transistor

-- Tech + Letter = God
combine (T Piston) (L Lambda) = G Nyx
combine (T Piston) (L Mu) = G Nyx
combine (T Piston) (L Omega) = G Hypnos
combine (T Transistor) (L Lambda) = G Thanatos
combine (T Transistor) (L Mu) = G Thanatos
combine (T Transistor) (L Omega) = G Nyx
combine (T Gear) (L Lambda) = G Thanatos
combine (T Gear) (L Mu) = G Thanatos
combine (T Gear) (L Omega) = G Thanatos

-- Tech + God = Letter
combine (T Piston) (G Nyx) = L Mu
combine (T Piston) (G Hypnos) = L Omega
combine (T Piston) (G Thanatos) = L Omega
combine (T Transistor) (G Nyx) = L Omega
combine (T Transistor) (G Hypnos) = L Omega
combine (T Transistor) (G Thanatos) = L Lambda
combine (T Gear) (G Nyx) = L Mu
combine (T Gear) (G Hypnos) = L Omega
combine (T Gear) (G Thanatos) = L Mu

-- Tech + Demon = Primary
combine (T Piston) (D Mammon) = P Green
combine (T Piston) (D Asmodeus) = P Blue
combine (T Piston) (D Belial) = P Red
combine (T Transistor) (D Mammon) = P Green
combine (T Transistor) (D Asmodeus) = P Red
combine (T Transistor) (D Belial) = P Red
combine (T Gear) (D Mammon) = P Green
combine (T Gear) (D Asmodeus) = P Red
combine (T Gear) (D Belial) = P Green

-- Tech + Element = Tech
combine (T Piston) (E Deuterium) = T Transistor
combine (T Piston) (E Erbium) = T Piston
combine (T Piston) (E Gold) = T Piston
combine (T Transistor) (E Deuterium) = T Gear
combine (T Transistor) (E Erbium) = T Gear
combine (T Transistor) (E Gold) = T Gear
combine (T Gear) (E Deuterium) = T Piston
combine (T Gear) (E Erbium) = T Piston
combine (T Gear) (E Gold) = T Piston

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
instance Random Substance where
    randomR (a,z) g = (toEnum (retval `mod` 21 ),g')
        where (retval,g') = randomR (fromEnum a ,fromEnum z ) g
    random = randomR (minBound,maxBound)

data SubWrapper a = SW {subGen :: StdGen
                       ,attributes :: a
                       ,cTree :: CTree a}

data CTree a = STNode {cVal :: SubWrapper a
                      ,cLeft :: CTree a
                      ,cRight :: CTree a
                        } 
             | STLeaf {cVal :: SubWrapper a}