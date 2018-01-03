module Smiley where

import DF
import List
import Text.Regex

smileyList = f <$> readFile "/home/mika/.perso/emoji_correspondance.csv"
  where f = map addstuff . parseCSV ','
        addstuff [a, b] = [a, ':' : b ++ ":"]
          where escapespecial = f "().^$?|"
                  where f [] s = s
                        f (x:xs) s = f xs $ replaceStr [x] ("\\"++[x]) s
-- .[{()\*+?|^$

substitute_smileys body = do
  smileys <- smileyList
  let f "" _ acc = acc
      f (x:xs) False acc = f xs (isBoundary x) $ acc ++ [x]
      f s@(x:xs) True acc = f s' (isBoundary $ last s'') (acc++s'')
        where (s'',s') = g smileys
              g [] = ([x],xs)
              g ([a, b]:xs) | beginWith a s && headBoundary s' = (b, s')
                            | otherwise = g xs
                where s' = drop (length a) s
      headBoundary "" = True
      headBoundary (x:xs) = isBoundary x
      isBoundary = (==Nothing) . matchRegex re . (:[])
      re = mkRegex "[a-zA-Z0-9]"
  return $ f body True ""

  -- replace <$> smileyList
  -- where replace = f True body 
  --       f :: Boolean -> String -> [[String]] -> String
  --       f False 
  --       -- f s [] = s
  --       -- f s ([a, b]:xs) = f (repl a b s) xs
  --       --   where repl a b s = subRegex re s $ "\\1" ++ b ++ "\\2"
  --       --         re = mkRegexWithOpts ("([^a-zA-Z0-9]?)" ++ a ++ "([^a-zA-Z0-9]?)") True False

-- substitute_smileys body =
--   replace <$> smileyList
--   where replace = f body 
--         f :: String -> [[String]] -> String
--         f s [] = s
--         f s ([a, b]:xs) = f (repl a b s) xs
--           where repl a b s = subRegex re s $ "\\1" ++ b ++ "\\2"
--                 re = mkRegexWithOpts ("([^a-zA-Z0-9]?)" ++ a ++ "([^a-zA-Z0-9]?)") True False

        
