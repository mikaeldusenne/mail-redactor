import Lib

import Control.Monad.State.Lazy

type St = State Int

    
-- test :: String -> String
-- test s = evalState (put 1 :: State Int String) $ s
--   -- where f l = do
--   --         (v, x) <- runState
--   --         put(x+1)
--   --         v <- evalState
--   --         return $ l ++ show x

-- test :: [String] -> Int
-- test s = (evalState $ (modify (+ 1) >> get)) 1
test s = runState . foldM (\acc e -> ((modify (+ 1) >> get)) $ e) (return "" :: State Int String)

main :: IO ()
-- main = readFile "data/mail_tester_body" >>= (show <$>) .run >>= error
main = (show . test . lines <$> readFile "data/test") >>= error
