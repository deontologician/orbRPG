{-# LANGUAGE OverloadedStrings #-}

module Game.OrbRPG.Formatting where

import Data.Text hiding (map, foldr, foldl, reverse)
import Prelude hiding (replicate, unlines, lines, words, unwords,
                       length, take, zipWith, init, head)
import qualified Prelude as P
import qualified Data.List as L
import Game.OrbRPG.Types


-- Creates a line of just ═ characters
line :: Int -> Text
line n = replicate n "═"

-- wraps text to the given line length (nonexhaustive match isn't a problem
-- because of the base value given to the fold)
--wrap :: Int -> Text -> Text
wrap w = unlines . P.reverse . L.foldl' go [""] . words
    where 
      go :: [Text] -> Text -> [Text]
      go [] _ = undefined -- this should never occur
      go (h:hs) next = if length h + length next + 1 > w
                       then next:h:hs
                       else (h `append` " " `append` next):hs

-- Only wraps a line if necessary.
wrapAsNeeded :: Int -> Text -> Text
wrapAsNeeded n = unlines . P.concatMap go . lines
    where go str = if length str <= n then [str]
                   else lines $ wrap n str

wrapScr :: Text -> Text
wrapScr = wrapAsNeeded 74
               
-- puts a box around a list of strings
titleBox :: Int -> Text -> Text -> Text
titleBox w title s = if length title + 2 > w 
                      then "" -- Better to do nothing than have a malformed box
                      else topper `append` emptyline 
                               `append` makemiddle `append`
                           emptyline `append` bottomer
                     
    where
      margin = 2
      wBig = w - 2 -- 2 characters for ║ and ║ on the sides
      wM = wBig - 2*margin -- width of the actual text - margins and ║s
      s' = lines . wrapAsNeeded wM $ s
      fixlen str = if length str <= wM
                   then str `append` replicate (wM - length str) " "
                   else take wM str
      emptyline = "║" `append` replicate wBig " " `append` "║\n"
      makemiddle = unlines $ P.map (\r -> "║  " `append` fixlen r `append` "  ║") s'
      topper = "╔" `append` centerT' "═" wBig title `append` "╗\n"
      bottomer = "╚" `append` line wBig `append` "╝\n"

-- Simple square box around text (autowrapped to 80 width)
boxer :: Text -> Text
boxer = titleBox 80 ""

-- Basic titleBox for a describable object
dBox :: (Describable a) => Int -> a -> Text
dBox n a = titleBox n (name a) (desc a)

-- This puts 2d strings next to each other separated by a space. All the
-- complexity is mostly dealing with non-uniform widths of lines in a string,
-- and different heights between the two strings to be put next to each other.
nextTo :: Text -> Text -> Text
x `nextTo` y = unlines $ P.zipWith (\l r -> l `append` " " `append` r) xlines ylines
    where 
      xlen = P.length . lines $ x
      ylen = P.length . lines $ y
      maxXlen = maxlen x
      maxYlen = maxlen y
      padX = P.map (\ln -> ln `append` 
                   replicate (maxXlen - length ln ) " ") (lines x)
      padY = P.map (\ln -> ln `append` 
                   replicate (maxYlen - length ln ) " ") (lines y)          
      xlines = if xlen >= ylen then padX 
               else padX ++ P.replicate (ylen - xlen) (replicate maxXlen " ")
      ylines = if ylen >= xlen then padY
               else padY ++ P.replicate (xlen - ylen) (replicate maxYlen " ")


-- | Max width of lines in a string
maxlen :: Text -> Int
maxlen = L.foldl' (\a b -> max a (length b)) 0 . lines

-- | This creates a width 1 string
myMod :: Int -> Int -> Int -> Int
myMod n m i = div n m + max 0 (mod n m - i)

twoBox :: (Describable a, Describable b) => Int -> a -> b -> Text
twoBox n a b = dBox aW a `nextTo` dBox bW b
    where n' = n - 1 -- account for space
          aW = myMod n' 2 0
          bW = myMod n' 2 1
           
-- Draws three dBoxes next to each other.
threeBox :: (Describable a, Describable b, Describable c) =>
            Int -> a -> b -> c -> Text
threeBox n a b c = dBox aW a `nextTo` dBox bW b `nextTo` dBox cW c
      where 
        n' = n - 2 -- account for spaces
        aW = myMod n' 3 0
        bW = myMod n' 3 1
        cW = myMod n' 3 2

-- Draws four dBoxes next to each other
fourBox :: (Describable a, Describable b, Describable c,Describable d) =>
           Int -> a -> b -> c -> d -> Text
fourBox n a b c d = dBox aW a `nextTo` dBox bW b 
                    `nextTo` dBox cW c `nextTo` dBox dW d
      where 
        n' = n - 3 -- account for spaces
        aW = myMod n' 4 0
        bW = myMod n' 4 1
        cW = myMod n' 4 2
        dW = myMod n' 4 3

boxList :: (Describable a) => Int -> [a] -> Text
boxList w (x:xs) = P.foldr (nextTo . dBox w) (dBox w x) xs
boxList _ _ = ""


-- centers text in a field of length w (identity if string is too long)
centerT' :: Text -> Int -> Text -> Text
centerT' c w "\n" = replicate w c
centerT' c w "" = replicate w c
centerT' c w t = init . unlines . map (center w (head c)) . lines $ t

centerT :: Int -> Text -> Text
centerT = centerT' " "

center' :: Int -> Text -> Text
center' n = center n ' '

-- Common center width
centerScr :: Text -> Text
centerScr = centerT 74

bulletList :: Text -> Text
bulletList = unlines . P.zipWith append (repeat "• ") . lines

indent' :: Int -> Text -> Text
indent' n = unlines . P.map (replicate n " " `append`) . lines

indent :: Text -> Text
indent = indent' 4
