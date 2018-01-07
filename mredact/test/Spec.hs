import Lib

main :: IO ()
main = readFile "data/mail_tester_body" >>= (show <$>) .run >>= error
