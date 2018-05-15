module MailRedactor.Utils where

import System.Random (randomIO)


randomID :: IO [Char]
randomID = (("yolo"++) . show . abs) <$> (randomIO :: IO Integer)

