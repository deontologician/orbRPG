module Formatting where

import qualified Data.Text as T
import Data.List
import Types


-- Creates a line of just ═ characters
line :: Int -> String
line n = replicate n '═'

-- Centers and text wraps a string and then puts a box around it
boxCWrap :: String -> String
boxCWrap = boxer . unlines . map (\r -> center 74 r) . lines . wrap 74

-- Just wraps the string and puts a box around it
boxWrap :: String -> String
boxWrap = boxer . wrap 74

-- puts a box around a list of strings
titleBox :: String -> String -> String
titleBox title s = topper ++ emptyline ++ makemiddle ++ emptyline ++ bottomer
    where
      s' = lines s
      fixlen str = if length str <= 74 
                   then str ++ (replicate (74 - length str) ' ')
                   else take 74 str
      emptyline = "║" ++ (replicate 78 ' ') ++ "║\n"
      makemiddle = unlines $ map (\r -> "║  " ++ fixlen r ++ "  ║") s'
      topper = "╔" ++ center' '═' 78 title ++ "╗\n"
      bottomer = "╚" ++ line 78 ++ "╝\n"

boxer :: String -> String
boxer = titleBox ""

descriptBox :: (Describable a) => a -> String
descriptBox d = titleBox (name d) (wrapScr . desc $ d)

-- wraps text to the given line length (nonexhaustive match isn't a problem
-- because of the base value given to the fold)
wrap :: Int -> String -> String
wrap w = unlines . reverse . foldl' go [""] . words
    where 
      go [] _ = undefined -- this should never occur
      go (h:hs) next = if length h + length next + 1 > w
                           then next:h:hs
                           else (h++" "++next):hs

wrapScr :: String -> String
wrapScr = wrap 74

-- centers text in a field of length w (identity if string is too long)
center' :: Char -> Int -> String -> String
center' c w "\n" = (replicate w c)
center' c w "" = (replicate w c)
center' c w  s = T.unpack . T.init . T.unlines .
              map (T.center w c) . T.lines $ T.pack s

center :: Int -> String -> String
center = center' ' '

centerScr :: String -> String
centerScr = center 74

cWrapScr :: String -> String
cWrapScr = centerScr . wrapScr

bulletList :: String -> String
bulletList = unlines . zipWith (++) (repeat "• ") . lines

indent :: String -> String
indent = unlines . map ("    "++) . lines
