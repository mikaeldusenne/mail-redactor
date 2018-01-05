{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Data.Set as Set

-- import qualified Codec.MIME.QuotedPrintable as QP
import QuotedPrintable
import Control.Lens hiding (element)

import Smiley

import System.Process
import System.IO
import System.Directory

import Control.Monad

import Data.List
import List

import Text.Regex.PCRE
import General
import Hunix (exec)

import Text.Pandoc

import Text.Pandoc.Readers.Markdown 
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Walk
import Skylighting
import System.Random (randomIO)

-- import qualified Data.ByteString.Char8 as C
-- import 

import qualified Data.Map.Strict as M

getFileMimeType :: String -> IO [Char]
getFileMimeType = (extract <$>) . exec "file" . (:["--mime-type"])
  where extract = trim . tail . dropWhile (/=':')

encode_base64 = exec "base64" . (:[])

data Mail = Mail {_plain :: String
                 ,_html :: String
                 ,_inlines :: [String]
                 ,_attachments :: [String]}
makeLenses ''Mail

instance Show Mail where
  show = createAlternative

instance Monoid Mail where
  mempty = Mail { _plain="", _html="", _inlines=[], _attachments=[] }
  mappend (Mail a b c d) (Mail a' b' c' d') = Mail (a++"\n"++a') (b++"\n"++b') (c++c') (d++d')

-- create_attachment :: String -> IO (Maybe Mail)
create_attachment path = doesFileExist path >>= (f ? return Nothing)
  where f = do
          mimeType <- getFileMimeType path
          b64 <- encode_base64 path
          id <- (("id"++) . show) <$> (randomIO :: IO Integer)
          let filename = basename path
              plaina = "["++mimeType++": " ++ filename ++ "@" ++ id ++ " ]"
              contentID = "Content-ID: <" ++ id ++ ">\n"
              contentType = ((("Content-Type: "++) . (++";")) $ mimeType)
                          ++ " name=\"" ++ filename ++ "\"\n"
              is_inline = beginWith "image" mimeType
              ans = mempty {_plain=plaina}
              atta disp= contentType
                ++ "Content-Disposition: "++ disp ++"; filename=\""++filename++"\"\n"
                ++ "Content-Transfer-Encoding: BASE64\n"
                ++ contentID
                -- ++ "X-Attachment-Id: <" ++ id ++ ">\n\n"
                ++ "\n"
                ++ b64
                
          return . Just $
            if is_inline
            then mempty {_html = "<img src=\"cid:" ++ id ++ "\" alt=\"image"++ id ++"\">"
                        ,_inlines = [atta "inline"]}
            else mempty{_attachments = [atta "attachment"]}

-- createMail :: [String] -> IO Mail
createMail ls = foldM f mempty ls
  where f mail l@('/':_) = (mappend mail . just_or_default (default_line l))
          <$> create_attachment (trim l)
        f mail l = return . mappend mail $ default_line l
        default_line l = mempty{_html=l, _plain=l}



-- createMultiparts :: [String] -> IO ([String], [String] ,[String])
-- createMultiparts ls = f ls [] [] []
--   where f [] html plain attachments = return (html, plain, attachments)
-- --        f (('§':xs):ls) h p a = do
--         f (l@('/':xs):ls) h p a = doesFileExist l >>= (attach !∫ default_line l ls h p a)
--           where attach = do
--                   mimeType <- getFileMimeType l
--                   b64 <- encode_base64 l
--                   -- print mimeType
--                   id <- (("id"++) . show) <$> (randomIO :: IO Integer)
--                   -- print id
--                   let is_image = beginWith "image" mimeType
--                       filename = basename l
--                       htmla=if is_image
--                             then "<img src=\"cid:" ++ id ++ "\" alt=\"image"++ id ++"\">"
--                             else ""
--                       plaina = "["++mimeType++": " ++ filename ++ "@" ++ id ++ " ]"
                      
--                       -- contentID = if is_image then "Content-ID: <" ++ id ++ ">\n" else ""
--                       contentID = "Content-ID: <" ++ id ++ ">\n"
--                       contentType = ((("Content-Type: "++) . (++";")) $ mimeType)
--                                     ++ " name=\"" ++ filename ++ "\"\n"
--                       contentDisposition = "Content-Disposition: "++disposition++"; filename=\"" ++ filename ++ "\"\n"
--                         where disposition = if is_image then "inline" else "attachment"
                        
--                       atta = contentType
--                         ++ contentDisposition
--                         ++ "Content-Transfer-Encoding: BASE64\n"
--                         ++ contentID
--                         ++ "X-Attachment-Id: <" ++ id ++ ">\n\n"
--                         ++ "\n"
--                         ++ b64
--                   f ls (h++[htmla]) (p++[plaina]) (a++[atta])

--                 -- return mime++content
--         f (l:ls) h p a = default_line l ls h p a
--         default_line l ls h p a = f ls (h++[l]) (p++[l]) a

-- data ContentType = Alternative | Mixed | Related | Plain | HTML | Attachment String

-- instance Show ContentType where
--   show Alternative = f "multipart/alternative"
--   show Mixed       = f "multipart/mixed"
--   show Related     = f "multipart/related"
--   show Plain       = f "text/plain"
--   show HTML        = f "text/html"
--   show Attachment  = f
--   where f s = "Content-Type: " ++ s ++ ";"

-- data Encoding = QuotedPrintable | Base64

-- data Mail = Multipart { content'type :: ContentType
--                            , boundary :: String
--                            , content :: [Mail]
--                            }

--           | Part = Part { content'transfer'encoding :: Encoding
--                         , }

createAlternative mail =
  unlines ["Content-Type: multipart/alternative; boundary=" ++ alternativeBound
          -- ,"MIME-version: 1.0"
          ,""
          ,alternativeBoundline ""
          ,"Content-Transfer-Encoding: quoted-printable"
          ,"Content-Type: text/plain; charset=\"UTF-8\";"
            -- ,"    format=flowed"
          ,""
          -- ,encode . _plain $ mail
          ,_plain mail
          ,""
          ,alternativeBoundline ""
          ,createMixed mail
          ,""
          ,alternativeBoundline "--"
          ]
  where alternativeBound = "alternativeboundary1234567890"
        alternativeBoundline add = "--"++alternativeBound++add

createMixed mail = unlines ["Content-Type: multipart/mixed;"
                           ,"    type=\"text/html\";"
                           ,"    boundary=" ++ mixedBoundary
                           ,""
                           ,mixedBoundaryline ""
                           ,createRelated mail
                           ,""
                           ,unlines
                             . insertBeforeEach (mixedBoundaryline "")
                             . _attachments
                             $ mail
                           ,""
                           ,mixedBoundaryline "--"
                           ]
  where mixedBoundary = "mixedboundary7894561230"
        mixedBoundaryline add = "--" ++ mixedBoundary ++ add



createRelated mail = unlines ["Content-Type: multipart/related;"
                             ,"    type=\"text/html\";"
                             ,"    boundary=" ++ relatedBoundary
                             ,""
                             ,relatedBoundaryline ""
                             ,"Content-Transfer-Encoding: quoted-printable"
                             ,"Content-Type: text/html; charset=\"UTF-8\";"
                             ,""
                             -- ,encode . _html $ mail
                             ,_html mail
                             ,""
                             ,unlines
                               . insertBeforeEach (relatedBoundaryline "")
                               . _inlines $ mail
                             ,""
                             ,relatedBoundaryline "--"
                             ]
  where relatedBoundary = "relatedBoundary7894561230"
        relatedBoundaryline add = "--" ++ relatedBoundary ++ add


run str = do
  ls <- lines <$> substitute_smileys str
  
  template <- readFile "/home/mika/.perso/pandoc.html"
  let readerExts = Set.union (Set.fromList [Ext_emoji]) $ readerExtensions def
      readerOpts = (def {readerStandalone = True,
                         readerExtensions = readerExts})
      writerOpts = (def {writerHighlight = True,
                         writerHighlightStyle = tango,
                         writerTemplate = Just template})
      compileHtml = writeHtmlString writerOpts . (\(Right e) -> e) . readMarkdown readerOpts
  
  mail <- over plain encode . over html (encode . compileHtml) <$> createMail ls

  print mail
  -- putStrLn $ createAlternative mail

main = getContents >>= run
