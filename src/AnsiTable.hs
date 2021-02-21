module AnsiTable
    ( newTable
    , withBorder
    , withHeading
    , withAlignments
    , Alignment (..)
    , Row (..)
    , cell
    , addRow
    , addRows
    , addHLine
    , (===)
    , ($+)
    , printTable
    ) where

import System.Console.ANSI
import Data.List (transpose, intercalate)
import Control.Monad (when)

data Cell = Cell
  { color   :: [SGR]
  , content :: String
  } deriving (Eq, Show)

type Width      = Int
data Row        = HLine | Cells [Cell] deriving (Eq, Show)
data Alignment  = AlignLeft | AlignRight

data AnsiTable = AnsiTable
  { border      :: Bool
  , heading     :: Bool
  , rows        :: [Row]
  , alignments  :: [Alignment]
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
  { color   = []
  , content = s
  }

addRow :: AnsiTable -> Row -> AnsiTable
addRow t r = t { rows = rows t ++ [r] }

addRows :: AnsiTable -> [Row] -> AnsiTable
addRows t rs = t { rows = rows t ++ rs }

addHLine :: AnsiTable -> AnsiTable
addHLine t = addRow t HLine

(===) :: AnsiTable -> Row -> AnsiTable
(===) = addRow

($+) :: AnsiTable -> (AnsiTable -> AnsiTable) -> AnsiTable
($+) t f = f t

-- get rows of cells only, now HLines
cellsOnly :: AnsiTable -> [[Cell]]
cellsOnly t = map remCells $ filter (/= HLine) $ rows t
  where remCells (Cells r)  = r

printTable :: AnsiTable -> IO ()
printTable t = do
  when (border t) (printHorizBorder colwidths "┌" "┬" "┐")
  if heading t then
    printRow (border t) colwidths lo header
  else
    printRow (border t) colwidths as header
  when (border t) (printHorizBorder colwidths "├" "┼" "┤")
  mapM_ (printRow (border t) colwidths as) rest
  when (border t) (printHorizBorder colwidths "└" "┴" "┘")
  where colwidths = map (maximum . map (length . content)) $ transpose cs
        cs        = map remCells $ filter (/= HLine) $ rows t
        remCells (Cells r)  = r
        header    = head $ rows t
        rest      = tail $ rows t
        as        = alignments t
        lo        = repeat AlignLeft

padString :: Alignment -> Width -> String -> String
padString AlignLeft w s   = s ++ replicate (w - length s) ' '
padString AlignRight w s  = replicate (w - length s) ' ' ++ s

printCell :: (Width,Cell,Alignment) -> IO ()
printCell (w,c,a) = putStr $ padString a w $ content c

printHorizBorder :: [Width] -> String -> String -> String -> IO ()
printHorizBorder ws l m r = do
  putStrLn $ l ++ line ++ r
  where line' = map (`replicate` '─') ws
        line  = intercalate m line'

printRow :: Bool -> [Width] -> [Alignment] -> Row -> IO ()
printRow border ws _ HLine = printHorizBorder ws "├" "┼" "┤"
printRow border ws as (Cells r) = do
  when border (putStr "│")
  mapM_ (\wc' -> printCell wc' >> putStr sep) $ init wca
  printCell $ last wca
  when border (putStr "│")
  putStrLn ""
  where wca = zip3 ws r as
        sep = if border then "│" else " "

