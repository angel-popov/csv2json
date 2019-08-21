module Main where

import Lib
import Control.Monad.Except
main :: IO ()
main = getContents >>= putStrLn . either ("ERROR:"++) id . toCSV
