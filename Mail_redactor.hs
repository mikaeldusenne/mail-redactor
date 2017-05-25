-- module Mail_redactor where

import System.Environment
import Data.List
import qualified Data.ByteString.Char8 as Ch8 
import System.Process
import System.IO

import List
import General
import Parse_mail
import Data.Char

getArgsOrContents :: IO String
getArgsOrContents = (safe_head <$> getArgs)
  >>= (\a -> case a of Nothing  -> getContents
                       Just ""  -> getContents
                       Just str -> return str)


modifyBS :: Ch8.ByteString -> IO Ch8.ByteString
modifyBS bs = do
  (_,Just fileh, _, _) <- createProcess (proc "mktemp" ["mail_redactor_XXXXXXXX"]){std_out = CreatePipe}
  file <- notrailingln <$> hGetContents fileh
--  print . map ord . notrailingln $ file
  Ch8.writeFile file bs
  (_,Just hout,_,_) <- createProcess (proc "vim" [file]){std_out = CreatePipe}
  hGetContents hout >>= hPutStrLn stderr
  Ch8.readFile file

main = do
  copypasta <- getArgsOrContents

  let mail_ID = selectHeader "message-id" . parseHeaders . lines $ copypasta
  
  mail_index <- fst . sliceOn '§' . head
    . filter (\e -> isInfixOf "INBOX" e && isInfixOf mail_ID e)
    . lines <$> readFile "/home/mika/.perso/mail_index"
  mail_content <- readFile mail_index

  writeFile "mail_original.txt" mail_content

  let Mail { getHeaders = h, getBody = b} = create_answer_mail $ mail_content
  newb <- modifyBS b
  putStrLn "-)-)-)-)"
  
  Ch8.putStrLn newb
  
