{-# LANGUAGE MultiParamTypeClasses #-}

module Combine where

data Substance = Null | Black | White 
               | P {p :: Primary} | L {l :: Letter} | G {g :: God} 
               | D {d :: Demon} | E {e :: Element} | T {t :: Tech}
                 deriving (Ord, Eq, Read, Show)


data Primary = Red | Green | Blue
               deriving (Ord, Eq, Read, Show)

data Letter = Lambda | Mu | Omega
            deriving (Ord, Eq)

data God = Nyx | Hypnos | Thanatos
         deriving (Ord, Eq, Read, Show)

data Demon = Asmodeus | Belial | Mammon
           deriving (Ord, Eq, Read, Show)

data Element = Deuterium | Erbium | Gold
             deriving (Ord, Eq, Read, Show)

data Tech = Piston | Transistor | Plastic
          deriving (Ord, Eq, Read, Show)

-- The Show and Read instances for Letters are a bit different
instance Show Letter where
    show Lambda = "[(λ)]"
    show Mu     = "[(μ)]"
    show Omega  = "[(Ω)]"

instance Read Letter where
    readsPrec _ value = 
        tryParse [("[(λ)]",Lambda),("[(μ)]",Mu),("[(Ω)]",Omega)]
        where tryParse [] = []
              tryParse ((attempt,result):xs) =
                  if (take (length attempt) value) == attempt
                  then [(result, drop (length attempt) value)]
                  else tryParse xs

-- Main combination function
combine :: Substance -> Substance -> Substance
combine _ Null = Null

combine Null Black = White
combine Null White = Black
combine Null _     = Null

combine Black Black = Black
combine Black White = Null

combine White _ 
