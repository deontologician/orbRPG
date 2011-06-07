module Game.OrbRPG.Formatting where

import qualified Data.Text as T
import Data.List (foldl')
import Game.OrbRPG.Types


-- Creates a line of just ═ characters
line :: Int -> String
line n = replicate n '═'

-- wraps text to the given line length (nonexhaustive match isn't a problem
-- because of the base value given to the fold)
wrap :: Int -> String -> String
wrap w = unlines . reverse . foldl' go [""] . words
    where 
      go [] _ = undefined -- this should never occur
      go (h:hs) next = if length h + length next + 1 > w
                       then next:h:hs
                       else (h++" "++next):hs

-- Only wraps a line if necessary.
wrapAsNeeded :: Int -> String -> String
wrapAsNeeded n = unlines . concatMap go . lines
    where go str = if length str <= n then [str]
                   else lines $ wrap n str

wrapScr :: String -> String
wrapScr = wrapAsNeeded 74
               
-- puts a box around a list of strings
titleBox :: Int -> String -> String -> String
titleBox w title s = if length title + 2 > w 
                      then "" -- Better to do nothing than have a malformed box
                      else topper ++ emptyline 
                               ++ makemiddle ++ 
                           emptyline ++ bottomer
                     
    where
      margin = 2
      wBig = w - 2 -- 2 characters for ║ and ║ on the sides
      wM = wBig - 2*margin -- width of the actual text - margins and ║s
      s' = lines . wrapAsNeeded wM $ s
      fixlen str = if length str <= wM
                   then str ++ replicate (wM - length str) ' '
                   else take wM str
      emptyline = "║" ++ replicate wBig ' ' ++ "║\n"
      makemiddle = unlines $ map (\r -> "║  " ++ fixlen r ++ "  ║") s'
      topper = "╔" ++ center' '═' wBig title ++ "╗\n"
      bottomer = "╚" ++ line wBig ++ "╝\n"

-- Simple square box around text (autowrapped to 80 width)
boxer :: String -> String
boxer = titleBox 80 ""

-- Basic titleBox for a describable object
dBox :: (Describable a) => Int -> a -> String
dBox n d = titleBox n (name d) (desc d)

-- This puts 2d strings next to each other separated by a space. All the
-- complexity is mostly dealing with non-uniform widths of lines in a string,
-- and different heights between the two strings to be put next to each other.
nextTo :: String -> String -> String
x `nextTo` y = unlines $ zipWith (\l r -> l ++ " " ++ r) xlines ylines
    where 
      xlen = length . lines $ x
      ylen = length . lines $ y
      maxXlen = maxlen x
      maxYlen = maxlen y
      padX = map (\ln -> ln ++ 
                   replicate (maxXlen - length ln ) ' ') (lines x)
      padY = map (\ln -> ln ++ 
                   replicate (maxYlen - length ln ) ' ') (lines y)          
      xlines = if xlen >= ylen then padX 
               else padX ++ replicate (ylen - xlen) (replicate maxXlen ' ')
      ylines = if ylen >= xlen then padY
               else padY ++ replicate (xlen - ylen) (replicate maxYlen ' ')


-- Max width of lines in a string
maxlen :: String -> Int
maxlen = foldl (\a b -> max a (length b)) 0 . lines

-- this is useful later
myMod :: Int -> Int -> Int -> Int
myMod n m i = div n m + max 0 $ mod n m - i

twoBox :: (Describable a, Describable b) => Int -> a -> b -> String
twoBox n a b = dBox aW a `nextTo` dBox bW b
    where n' = n - 1 -- account for space
          aW = myMod n' 2 0
          bW = myMod n' 2 1
           
-- Draws three dBoxes next to each other.
threeBox :: (Describable a, Describable b, Describable c) =>
            Int -> a -> b -> c -> String
threeBox n a b c = dBox aW a `nextTo` dBox bW b `nextTo` dBox cW c
      where 
        n' = n - 2 -- account for spaces
        aW = myMod n' 3 0
        bW = myMod n' 3 1
        cW = myMod n' 3 2

-- Draws four dBoxes next to each other
fourBox :: (Describable a, Describable b, Describable c,Describable d) =>
           Int -> a -> b -> c -> d -> String
fourBox n a b c d = dBox aW a `nextTo` dBox bW b 
                    `nextTo` dBox cW c `nextTo` dBox dW d
      where 
        n' = n - 3 -- account for spaces
        aW = myMod n' 4 0
        bW = myMod n' 4 1
        cW = myMod n' 4 2
        dW = myMod n' 4 3

boxList :: (Describable a) => Int -> [a] -> String
boxList w (x:xs) = foldr (nextTo . dBox w) (dBox w x) xs
boxList _ _ = ""


-- centers text in a field of length w (identity if string is too long)
center' :: Char -> Int -> String -> String
center' c w "\n" = replicate w c
center' c w "" = replicate w c
center' c w  s = T.unpack . T.init . T.unlines .
              map (T.center w c) . T.lines $ T.pack s

center :: Int -> String -> String
center = center' ' '

-- Common center width
centerScr :: String -> String
centerScr = center 74

bulletList :: String -> String
bulletList = unlines . zipWith (++) (repeat "• ") . lines

indent' :: Int -> String -> String
indent' n = unlines . map (replicate n ' ' ++) . lines

indent :: String -> String
indent = indent' 4
