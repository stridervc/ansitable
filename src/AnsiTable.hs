module AnsiTable
    ( newTable
    , withBorder
    , withHeading
    , withAlignments
    , Alignment (..)
    , cell
    , printTable
    , (===)
    , ($+)
    ) where

import System.Console.ANSI
import Data.List (transpose, intercalate)
import Control.Monad (when)

type Row    = [Cell]
type Width  = Int

data Alignment  = AlignLeft | AlignRight

data AnsiTable = AnsiTable
  { border      :: Bool
  , heading     :: Bool
  , rows        :: [Row]
  , alignments  :: [Alignment]
  }

data Cell = Cell
  { color   :: [SGR]
  , content :: String
  }

newTable :: AnsiTable
newTable = AnsiTable
  { border      = False
  , heading     = False
  , rows        = []
  , alignments  = repeat AlignLeft
  }

withBorder :: AnsiTable -> AnsiTable
withBorder t = t { border = True }

withHeading :: AnsiTable -> AnsiTable
withHeading t = t { heading = True }

withAlignments :: [Alignment] -> AnsiTable -> AnsiTable
withAlignments as t = t { alignments = as ++ repeat AlignLeft }

cell :: String -> Cell
cell s = Cell
  { color = []
  , content = s
  }

printTable :: AnsiTable -> IO ()
printTable t = do
  when (border t) (printHorizBorder colwidths "┌" "┬" "┐")
  if (heading t) then
    printRow (border t) colwidths lo header
  else
    printRow (border t) colwidths as header
  when (heading t) (printHorizBorder colwidths "├" "┼" "┤")
  mapM_ (printRow (border t) colwidths as) rest
  when (border t) (printHorizBorder colwidths "└" "┴" "┘")
  where colwidths = map (maximum . (map (length . content))) $ transpose $ rows t
        header    = head $ rows t
        rest      = tail $ rows t
        as        = alignments t
        lo        = repeat AlignLeft

(===) :: AnsiTable -> Row -> AnsiTable
(===) t r = t { rows = rows t ++ [r] }

($+) :: AnsiTable -> (AnsiTable -> AnsiTable) -> AnsiTable
($+) t f = f t

padString :: Alignment -> Width -> String -> String
padString AlignLeft w s   = s ++ (take (w - length s) $ repeat ' ')
padString AlignRight w s  = (take (w - length s) $ repeat ' ') ++ s

printCell :: (Width,Cell,Alignment) -> IO ()
printCell (w,c,a) = putStr $ padString a w $ content c

printHorizBorder :: [Width] -> String -> String -> String -> IO ()
printHorizBorder ws l m r = do
  putStrLn $ l ++ line ++ r
  where line' = map (\x -> take x (repeat '─')) ws
        line  = intercalate m line'

printRow :: Bool -> [Width] -> [Alignment] -> Row -> IO ()
printRow border ws as r = do
  when border (putStr "│")
  mapM_ (\wc' -> printCell wc' >> putStr sep) $ init wca
  printCell $ last wca
  when border (putStr "│")
  putStrLn ""
  where wca = zip3 ws r as
        sep = if border then "│" else " "

