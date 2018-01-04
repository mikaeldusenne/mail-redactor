module Main where

import qualified Data.Set as Set

import Smiley

import System.Process
import System.IO
import System.Directory

import List
import Text.Regex.PCRE
import General (alpha_num, (!∫))
import Hunix (exec)

import Text.Pandoc

import Text.Pandoc.Readers.Markdown 
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Walk
import Skylighting
import System.Random (randomIO)

import qualified Data.Map.Strict as M

getFileMimeType :: String -> IO [Char]
getFileMimeType = (extract <$>) . exec "file" . (:["--mime-type"])
  where extract = trim . tail . dropWhile (/=':')

encode_base64 = exec "base64" . (:[])

createMultiparts :: [String] -> IO ([String], [String] ,[String])
createMultiparts ls = f ls [] [] []
  where f [] html plain attachments = return (html, plain, attachments)
--        f (('§':xs):ls) h p a = do
        f (l@('/':xs):ls) h p a = doesFileExist l >>= (attach !∫ default_line l ls h p a)
          where attach = do
                  mimeType <- getFileMimeType l
                  b64 <- encode_base64 l
                  -- print mimeType
                  id <- show <$> (randomIO :: IO Integer)
                  -- print id
                  let is_image = beginWith "image" mimeType
                      filename = basename l
                      htmla=if is_image
                            then "<img src=\"cid:" ++ id ++ "\" alt=\"image"++ id ++"\">"
                            else ""
                      plaina = "["++mimeType++": " ++ id ++ " ]"
                      
                      -- contentID = if is_image then "Content-ID: <" ++ id ++ ">\n" else ""
                      contentID = "Content-ID: <id" ++ id ++ ">\n"
                      contentType = ((("Content-Type: "++) . (++";")) $ mimeType)
                                    ++ " name=\"" ++ filename ++ "\"\n"
                      contentDisposition = "Content-Disposition: "++disposition++"; filename=\"" ++ filename ++ "\"\n"
                        where disposition = if is_image then "inline" else "attachment"
                        
                      atta = contentType
                        ++ contentDisposition
                        ++ "Content-Transfer-Encoding: BASE64\n"
                        ++ contentID
                        -- ++ "X-Attachment-Id: <" ++ id ++ ">\n\n"
                        ++ "\n"
                        ++ b64
                  f ls (h++[htmla]) (p++[plaina]) (a++[atta])

                -- return mime++content
        f (l:ls) h p a = default_line l ls h p a
        default_line l ls h p a = f ls (h++[l]) (p++[l]) a

createAlternative html plain attachments =
  unlines ["Content-Type: multipart/alternative; boundary=" ++ alternativeBound
          ,"MIME-version: 1.0"
          ,""
          ,alternativeBoundline ""
          ,"Content-Transfer-Encoding: quoted-printable"
          ,"Content-Type: text/plain;"
          ,"    charset=UTF-8;"
            -- ,"    format=flowed"
          ,""
          ,plain
          ,""
          ,alternativeBoundline ""
          ,createMixed html attachments
          ,alternativeBoundline "--"
          ]
  where alternativeBound = "alternativeboundary1234567890"
        alternativeBoundline add = "--"++alternativeBound++add

createRelated html = unlines ["Content-Type: multipart/related;"
                             ,"    type=text/html;"
                             ,"    boundary=" ++ relatedBoundary
                             ,""
                             ,relatedBoundaryline ""
                             ,"Content-Transfer-Encoding: quoted-printable"
                             ,"Content-Type: text/html;"
                             ,"    charset=UTF-8;"
                             ,""
                             ,html
                             -- ,createAlternative html plain
                             -- ,""
                             -- ,unlines . insertBeforeEach (relatedBoundaryline "") $ attachts
                             ,relatedBoundaryline "--"
                             ]
  where relatedBoundary = "relatedBoundary7894561230"
        relatedBoundaryline add = "--" ++ relatedBoundary ++ add


createMixed html attachts = unlines ["Content-Type: multipart/mixed;"
                                    ,"    type=text/html;"
                                    ,"    boundary=" ++ mixedBoundary
                                    ,""
                                    ,mixedBoundaryline ""
                                    ,createRelated html
                                    ,""
                                    ,unlines . insertBeforeEach (mixedBoundaryline "") $ attachts
                                    ,mixedBoundaryline "--"
                                    ]
  where mixedBoundary = "mixedBoundary7894561230"
        mixedBoundaryline add = "--" ++ mixedBoundary ++ add



main = do
  ls <- lines <$> (getContents >>= substitute_smileys)
  css <- readFile "/home/mika/.perso/pandoc.css" -- "pandoc.css"
  -- r <- encode_base64 "/home/mika/Documents/random/lambda.png"
  -- let (h, p, a) = createMultiparts ls
  -- attachments <- sequence a
  (h, p, attachments) <- createMultiparts ls
  template <- readFile "/home/mika/.perso/pandoc.html"
  let md_str = unlines h
      plain = unlines p
      readerExts = Set.union (Set.fromList [Ext_emoji]) $ readerExtensions def
      readerOpts = (def {readerStandalone = True,
                         readerExtensions = readerExts})
      writerOpts = (def {writerHighlight = True,
                         writerHighlightStyle = tango,
                         writerTemplate = Just template})
      (Right pandoc) = readMarkdown
                       readerOpts
                       md_str
      html_str = writeHtmlString writerOpts pandoc
      -- html_str = unlines ["<!DOCTYPE html><html><head>",
      --                     "<meta charset=\"UTF-8\">",
      --                     "<style>",
      --                     concat . lines $ css,
      --                     "</style></head><body>",
      --                     body_html_str,
      --                     "</body></html>"]

  writeFile "/home/mika/tmp/mail_body.txt" html_str
  putStrLn $ createAlternative html_str plain attachments




-- pandoc writer for blockquote
-- surround with div?
-- { pandocWalker b@(BlockQuote l ) = Div ("",[],[]) [b]; w e = e }
