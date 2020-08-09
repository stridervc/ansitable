module AnsiTable
    ( newTable
    , withBorder
    , withHeading
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

cell :: String -> Cell
cell s = Cell
  { color = []
  , content = s
  }

printTable :: AnsiTable -> IO ()
printTable t = do
  when (border t) (printHorizBorder colwidths "┌" "┬" "┐")
  printRow (border t) colwidths header
  when (heading t) (printHorizBorder colwidths "├" "┼" "┤")
  mapM_ (printRow (border t) colwidths) rest
  when (border t) (printHorizBorder colwidths "└" "┴" "┘")
  where colwidths = map (maximum . (map (length . content))) $ transpose $ rows t
        header    = head $ rows t
        rest      = tail $ rows t

(===) :: AnsiTable -> Row -> AnsiTable
(===) t r = t { rows = rows t ++ [r] }

($+) :: AnsiTable -> (AnsiTable -> AnsiTable) -> AnsiTable
($+) t f = f t

padString :: Width -> String -> String
padString w s = s ++ (take (w - length s) $ repeat ' ')

printCell :: (Width,Cell) -> IO ()
printCell (w,c) = putStr $ padString w $ content c

printHorizBorder :: [Width] -> String -> String -> String -> IO ()
printHorizBorder ws l m r = do
  putStrLn $ l ++ line ++ r
  where line' = map (\x -> take x (repeat '─')) ws
        line  = intercalate m line'

printRow :: Bool -> [Width] -> Row -> IO ()
printRow border ws r = do
  when border (putStr "│")
  mapM_ (\wc' -> printCell wc' >> putStr sep) $ init wc
  printCell $ last wc
  when border (putStr "│")
  putStrLn ""
  where wc  = zip ws r
        sep = if border then "│" else " "

