module Main where

import AnsiTable

main :: IO ()
main = do
  let t = newTable
            $+ withBorder
            $+ withHeading
            $+ withAlignments [AlignLeft, AlignRight]
            === [cell "Name", cell "Amount"]
            === [cell "John Doe", cell "123456789"]
            === [cell "Jane Doe", cell "4567"]

  printTable t
