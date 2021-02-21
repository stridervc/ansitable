module Main where

import AnsiTable

main :: IO ()
main = do
  let t = newTable
            $+ withBorder
            $+ withHeading
            $+ withAlignments [AlignLeft, AlignRight]
            === Cells [cell "Name", cell "Amount"]
            === Cells [cell "John Doe", cell "123456789"]
            === Cells [cell "Jane Doe", cell "4567"]

  printTable t
