module Formatting where

import qualified Data.Text as T
import Data.List


-- Creates a line of just ═ characters
line :: Int -> String
line n = replicate n '═'

-- Centers and text wraps a string and then puts a box around it
boxCWrap :: String -> String
boxCWrap = boxer . map (\r -> center 74 r) . wrap 74

-- Just wraps the string and puts a box around it
boxWrap :: String -> String
boxWrap = boxer . wrap 74

-- puts a box around a list of strings
boxer :: [String] -> String
boxer s = topper ++ emptyline ++ makemiddle ++ emptyline ++ bottomer
    where
      fixlen str = if length str <= 74 
                   then str ++ (replicate (74 - length str) ' ')
                   else take 74 str
      emptyline = "║" ++ (replicate 78 ' ') ++ "║\n"
      makemiddle = unlines $ map (\r -> "║  " ++ fixlen r ++ "  ║") s
      topper = "╔" ++ line 78 ++ "╗\n"
      bottomer = "╚" ++ line 78 ++ "╝\n"

-- wraps text to the given line length (nonexhaustive match isn't a problem
-- because of the base value given to the fold)
wrap :: Int -> String -> [String]
wrap w = reverse . foldl' go [""] . words
    where 
      go [] _ = undefined -- this should never occur
      go (h:hs) next = if length h + length next + 1 > w
                           then next:h:hs
                           else (h++" "++next):hs

-- centers text in a field of length w (identity if string is too long)
center :: Int -> String -> String
center w "\n" = (replicate w ' ')
center w "" = (replicate w ' ')
center w  s = T.unpack . T.init . T.unlines .
              map (T.center w ' ') . T.lines $ T.pack s
