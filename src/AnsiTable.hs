module AnsiTable
    ( Cell (..)
    , Alignment (..)
    , cellFromString
    , showStringTable
    , showTable
    ) where

import Data.List
import System.Console.ANSI

type Width = Int
data Alignment = AlignLeft | AlignRight

data Cell = Cell { sgr :: [SGR]
                 , content :: String
                 , align :: Alignment
                 }

cellFromString :: String -> Cell
cellFromString cs = Cell { sgr = [Reset]
                         , content = cs
                         , align = AlignLeft
                         }

maxLength :: [Cell] -> Width
maxLength = foldl (\acc x -> if length (content x) > acc then length (content x) else acc) 0

zipWidths :: [Width] -> [Cell] -> [(Cell, Width)]
zipWidths ws cs = zip cs ws

putContents :: (Cell, Width) -> IO ()
putContents (c,w) = do
  putStr "│ "
  setSGR $ sgr c
  putStr $ s
  putStr $ replicate (w+1-length s) ' '
  setSGR [ Reset ]
  where s = content c

putContentsLine :: [(Cell, Width)] -> IO ()
putContentsLine cs = do
  mapM_ putContents cs
  putStrLn "│"

putTopBorder :: [Width] -> IO ()
putTopBorder cs = do
  let hlines = map (\w -> replicate (w+2) '─') cs
  putStr "┌"
  putStr $ intercalate "┬" hlines
  putStrLn "┐"

putBottomBorder :: [Width] -> IO ()
putBottomBorder cs = do
  let hlines = map (\w -> replicate (w+2) '─') cs
  putStr "└"
  putStr $ intercalate "┴" hlines
  putStrLn "┘"

putMiddleBorder :: [Width] -> IO ()
putMiddleBorder cs = do
  let hlines = map (\w -> replicate (w+2) '─') cs
  putStr "├"
  putStr $ intercalate "┼" hlines
  putStrLn "┤"

showTable :: [[Cell]] -> IO ()
showTable t = do
  putTopBorder widths
  mapM_ putContentsLine $ [head z]
  putMiddleBorder widths
  mapM_ putContentsLine $ tail z
  putBottomBorder widths

  where tt = transpose t
        widths = map maxLength tt
        z = map (zipWidths widths) t

showStringTable :: [[String]] -> IO ()
showStringTable = showTable . map (map cellFromString)

{--
 - (H1, C1W), (H2, C2W)
 - (V1, C1W), (V2, C2W)
 --}
 
{--
a = [ ["H1", "H2", "H3"]
    , ["V1", "V2", "V3"]
    ]
--}
