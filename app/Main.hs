module Main where

import AnsiTable

main :: IO ()
main = do
  let t = newTable
            $+ withBorder
            $+ withHeading
            === [cell "Column 1", cell "Column 2"]
            === [cell "1,2", cell "2,2"]
            === [cell "data", cell "some more data"]

  printTable t
