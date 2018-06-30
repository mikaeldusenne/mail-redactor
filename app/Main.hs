module Main where

import Lib
import System.Environment

main = do
  args <- getArgs
  let debug = any (=="debug") args
  getContents >>= run debug >>= (putStrLn . show)

