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

putPadded :: (Cell, Width) -> IO ()
putPadded (c,w) = do
  setSGR $ sgr c
  putStr $ s
  putStr $ replicate (w+1-length s) ' '
  setSGR [ Reset ]
  where s = content c

putPaddedLine :: [(Cell, Width)] -> IO ()
putPaddedLine cs = do
  mapM_ putPadded cs
  putStrLn ""

showTable :: [[Cell]] -> IO ()
showTable t = do
  mapM_ putPaddedLine z
  where tt = transpose t
        widths = map maxLength tt
        z = map (zipWidths widths) t

showStringTable :: [[String]] -> IO ()
showStringTable t = do
  mapM_ putPaddedLine z
  where tt = transpose ct
        widths = map maxLength tt
        z = map (zipWidths widths) ct
        ct = map (map cellFromString) t

{--
 - (H1, C1W), (H2, C2W)
 - (V1, C1W), (V2, C2W)
 --}
 
{--
a = [ ["H1", "H2", "H3"]
    , ["V1", "V2", "V3"]
    ]
--}
