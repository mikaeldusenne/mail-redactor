module Main where

import System.Process
import System.IO

import List
import Text.Regex.PCRE
import General (alpha_num)
import Text.Pandoc

import Text.Pandoc.Readers.Markdown 
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Walk
import Skylighting
import System.Random (randomIO)

import qualified Data.Map.Strict as M

-- import Text.Pandoc.Highlighting

-- class Bounded a where
--   getBoundary :: String
--   getBoundaryOpen e = "--" ++ getBoundary e ++ "\n"
--   getBoundaryClosed e = "--" ++ getBoundary e ++ "--\n"

-- data Multipart_Attachment = error
-- data Multipart_Html = Multipart_Html 

-- data Multipart_Mixed = MMixed { getRelated :: Multipart_Related,
--                             getAttachments :: [Multipart_Attachment],
--                             get}

-- instance Bounded Multipart_Mixed where
--   getBoundary = "hereisthemixedboundary0123456789"
  

-- data Multipart_Related = MRelated { getAlternative :: Multipart_Alternative,
--                                              getInlineAttachments :: [Multipart_Inline] }

-- instance Bounded Multipart_Related where
--   getBoundary = "hereistherelatedboundary4116478662"

-- data Multipart_Alternative = MAlternative {getPlain :: Multipart_plain,
--                                            getHTML :: Multipart_html}

-- instance Bounded Multipart_Alternative where
--   getBoundary = "hereisthealternativeboundary9876543210"


getFileMimeType path_to_file = ((\(_,Just out,_,_)->out) <$> createProcess (proc "file" ["--mime-type",path_to_file]){std_out = CreatePipe})
  >>= ((extract<$>) <$> hGetContents)
  where extract (':':' ':xs) = notrailingln xs
        extract (x:xs) = extract xs
        
--  where cmd = "file --mime-type ~/Documents/random/lambda.png | sed 's/^.*: \\(.*\\)$/\1/'"
------------------
-- getFileMimeType path_to_file = (\(_,Just r,_,_) -> r) <$> createProcess (shell cmd){std_out = CreatePipe}
--   >>= hGetContents
--   where cmd = "file --mime-type ~/Documents/random/lambda.png | sed 's/^.*: \\(.*\\)$/\1/'"
-----------------
-- getFileMimeType path_to_file = do
--   (_,Just out1,_,ph1) <- createProcess (proc "file" ["--mime-type",path_to_file]){std_out = CreatePipe}
-- --  >>= hGetContents
--   (_,Just out2,_,ph2) <- createProcess (proc "sed" ["s/^.*: \\(.*\\)$/\1/"]){std_out = CreatePipe,std_in=UseHandle out1}
--   waitForProcess ph1
--   waitForProcess ph2
  
--   hGetContents out2
-- --  where cmd = "file --mime-type ~/Documents/random/lambda.png | sed 's/^.*: \\(.*\\)$/\1/'"
  
  
encode_base64 path_to_file = (\(_,Just r,_,_) -> r) <$> createProcess (proc "base64" [path_to_file]){std_out = CreatePipe}
  >>= hGetContents

pandoc html = html

-- createMultiparts ls = f ls [] [] []
--   where f [] html plain attachments = (pandoc html, plain, attachments)
--         f (('§':xs):ls) h p a = f ls (h++[htmla]) (p++[plaina]) (a++[atta])
--           where htmla="<img src=\"cid:" ++ xs ++ "\" alt=\"image html\">"
--                 plaina = "[image: " ++ xs ++ " ]"
--                 atta=((("Content-Type: "++) . (++";"))
--                        <$>getFileMimeType xs)
--                      >>= (\ mime -> ((++)(mime ++ " name=\"" ++ xs ++ "\"\n"
--                                           ++ "Content-Disposition: inline; filename=\"" ++ xs ++ "\"\n"
--                                           ++ "Content-Transfer-Encoding: base64\n"
--                                           ++ "Content-ID: <" ++ xs ++ ">\n"
--                                           ++ "X-Attachment-Id: <" ++ xs ++ ">\n\n"))
--                                     <$> encode_base64 xs)
--         f (l:ls) h p a = f ls (h++[l]) (p++[l]) a



createMultiparts :: [String] -> IO ([String], [String] ,[String])
createMultiparts ls = f ls [] [] []
  where f [] html plain attachments = return (pandoc html, plain, attachments)
        f (('§':xs):ls) h p a = do
          mimeType <- getFileMimeType xs
          b64 <- encode_base64 xs
          -- print mimeType
          id <- show <$> (randomIO :: IO Integer)
          -- print id
          let is_image = beginWith "image" mimeType
          let htmla=if is_image
                    then "<img src=\"cid:" ++ id ++ "\" alt=\"image"++ id ++"\">"
                    else ""
              plaina = "["++mimeType++": " ++ id ++ " ]"
              disposition = if is_image then "inline" else "attachment"
              atta = ((("Content-Type: "++) . (++";")) $ mimeType)
                ++ " name=\"" ++ xs ++ "\"\n"
                ++ "Content-Disposition: "++disposition++"; filename=\"" ++ xs ++ "\"\n"
                ++ "Content-Transfer-Encoding: base64\n"
                ++ "Content-ID: <" ++ id ++ ">\n"
                ++ "X-Attachment-Id: <" ++ id ++ ">\n\n"
                ++ b64
          f ls (h++[htmla]) (p++[plaina]) (a++[atta])

                -- return mime++content
        f (l:ls) h p a = f ls (h++[l]) (p++[l]) a
-- (++) . sequence $
--                      [,
--                       ]
--          | l =~ "!\[[^\]]*\]"



insertBeforeEach e = concatMap (\a -> [e, a])

createAlternative html plain = unlines ["Content-Type: multipart/alternative; boundary=" ++ alternativeBound
                                       ,""
                                       ,alternativeBoundline ""
                                       ,"Content-Type: text/plain; charset=UTF-8"
                                       ,"Content-Transfer-Encoding: quoted-printable"
                                       ,""
                                       ,plain
                                       ,""
                                       ,alternativeBoundline ""
                                       ,"Content-Type: text/html; charset=UTF-8"
                                       ,""
                                       ,html
                                       ,alternativeBoundline "--"
                                       ]
  where alternativeBound = "alternativeboundary1234567890"
        alternativeBoundline add = "--"++alternativeBound++add

createRelated html plain attachts = unlines ["Content-Type: multipart/related; boundary=" ++ relatedBoundary
                                             ,""
                                             ,relatedBoundaryline ""
                                             ,createAlternative html plain
                                             ,""
                                             ,unlines . insertBeforeEach (relatedBoundaryline "") $ (map createAttachment attachts)
                                             ,relatedBoundaryline "--"
                                             ]
  where relatedBoundary = "relatedBoundary7894561230"
        relatedBoundaryline add = "--" ++ relatedBoundary ++ add

createAttachment a = a
        




main = do
  ls <- lines <$> getContents
  css <- readFile "/home/mika/.perso/pandoc.css" -- "pandoc.css"
  -- r <- encode_base64 "/home/mika/Documents/random/lambda.png"
  -- let (h, p, a) = createMultiparts ls
  -- attachments <- sequence a
  (h, p, attachments) <- createMultiparts ls
  let md_str = unlines h
      plain = unlines p
      (Right pandoc) = readMarkdown
                       (def {readerStandalone = True})
                       md_str
      body_html_str = writeHtmlString (def {writerHighlightStyle = pygments}) pandoc
      html_str = unlines ["<!DOCTYPE html><html><head>",
                          "<meta charset=\"UTF-8\">",
                          "<style>",
                          concat . lines $ css,
                          "</style></head><body>",
                          body_html_str,
                          "</body></html>"]
                          
  writeFile "/home/mika/tmp/mail_body.txt" body_html_str
  putStrLn $  createRelated html_str plain attachments




-- pandoc writer for blockquote
-- surround with div?
-- { pandocWalker b@(BlockQuote l ) = Div ("",[],[]) [b]; w e = e }
