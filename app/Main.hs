module Main where

import Lib

main = getContents >>= (show <$>) . run >>= putStrLn
