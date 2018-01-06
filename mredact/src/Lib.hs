{-# LANGUAGE TemplateHaskell, DuplicateRecordFields #-}
module Lib where

-----------------------------IMPORTS---------------------------------
import qualified Data.Set as Set

-- import qualified Codec.MIME.QuotedPrintable as QP
import QuotedPrintable
import Control.Lens hiding (element)

import Smiley
import List

import Text.Regex.PCRE
import General
import Hunix (exec)
import ContentType

import System.Process
import System.IO
import System.Directory

import Control.Monad

import Data.List

import Text.Pandoc hiding (Plain)

import Text.Pandoc.Readers.Markdown 
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Walk
import Skylighting
import System.Random (randomIO)


import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M
-- import qualified Data.ByteString.Char8 as C
--------------------------------------------------------------------

getFileMimeType :: String -> IO [Char]
getFileMimeType = (extract <$>) . exec "file" . (:["--mime-type"])
  where extract = trim . tail . dropWhile (/=':')

encode_base64 = exec "base64" . (:[])

randomID = (("id"++) . show . abs) <$> (randomIO :: IO Integer)

data TransferEncoding = B64 | QP

data PJ = PJ { oid :: String
             , mimetype :: ContentType
             , disposition :: String
             , name :: String
             , content :: String }

data Mail = Mail {_plain :: String
                 ,_html :: String
                 ,_pj :: [PJ]}
makeLenses ''Mail

instance Show TransferEncoding where
  show e = case e of
             B64 -> f "base64"
             QP  -> f "quoted-printable"
    where f s = "Content-Transfer-Encoding: " ++ s

-- instance Show Mail where
--   show = createAlternative

instance Monoid Mail where
  mempty = Mail { _plain="", _html="", _pj=[]}
  mappend (Mail a b c) (Mail a' b' c') = Mail (a++"\n"++a') (b++"\n"++b') (c++c')

instance Show PJ where
  show (PJ {oid=id, mimetype=m, disposition=d, name=n, content=b64}) = unlines [
    show m ++"; name=\"" ++ n ++ "\""
    ,"Content-Disposition: "++ d ++"; filename=\""++ n ++"\""
    ,show B64
    ,"Content-ID: <" ++ id ++ ">"
    ,"X-Attachment-Id: " ++ id
    ,"\n" ++ b64]

defaultPJ = PJ{oid="", mimetype=ContentType (MIME "") M.empty, disposition="", name="", content=""} :: PJ

-- create_attachment :: String -> IO (Maybe Mail)
create_attachment path = doesFileExist path >>= (f ? return Nothing)
  where f = do
          mimeType <- getFileMimeType path
          b64 <- encode_base64 path
          id <- randomID
          let filename = basename path
              plaina = "["++mimeType++": " ++ filename ++ "@" ++ id ++ " ]"
              is_inline = beginWith "image" mimeType
              ans = mempty {_plain=plaina}
              pj = defaultPJ {oid=id,
                              mimetype=defaultContentType{_CT=MIME mimeType},
                              name=filename,
                              content=b64}

          return . Just $
            if is_inline
            then ans {_html = "<div><img src=\"cid:" ++ id ++ "\""
                              ++" alt=\"image"++id++"\""
                              ++" data-inline-image=\"true\""
                              ++" width=\"416\" height=\"260\""
                              ++"></div>"
                        ,_pj = [pj{disposition="inline"}]}
            else ans {_pj = [pj{disposition="attachment"}]}

-- createMail :: [String] -> IO Mail
createMail ls = foldM f mempty ls
  where f mail l@('/':_) = (mappend mail . just_or_default (default_line l))
          <$> create_attachment (trim l)
        f mail l = return . mappend mail $ default_line l
        default_line l = mempty{_html=l, _plain=l}


data Element = Multipart {_content'type :: ContentType
                          ,_elemlist :: [Element]}
             | Text { _content'type :: ContentType,
                      _s ::  String }
             | PJE PJ
makeLenses ''Element

instance Show Element where
  show (PJE pj) = show pj
  show (Text {_content'type=t, _s=s}) = unlines
    [show QP
    ,show . over args (M.insert "charset" "utf-8") $ t
    ,"", s]
  show (Multipart {_content'type=t@(ContentType ct m),
                   _elemlist=l}) = unlines
    [show t,""
    ,unlines . insertBeforeEach ("--"++b) . map show $ l
    ,"", "--"++b++"--"]
    where b = m M.! "boundary"

newMultipart :: CT -> IO Element
newMultipart t = randomID >>= \s ->
  let m = M.fromList [("boundary",ctStr t ++ s)]
  in return Multipart{ _content'type=ContentType t m,
                       _elemlist=[]}

createAlternative mail =
  unlines ["Content-Type: multipart/alternative; boundary=" ++ alternativeBound
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
                             . fmap show
                             . filter ((=="attachment") . disposition)
                             $ _pj  mail
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
                               . fmap show
                               . filter ((=="inline") . disposition)
                               . _pj $ mail
                             ,""
                             ,relatedBoundaryline "--"
                             ]
  where relatedBoundary = "relatedBoundary7894561230"
        relatedBoundaryline add = "--" ++ relatedBoundary ++ add


run str = do
  ls <- lines <$> substitute_smileys str
  template <- readFile "/home/mika/.perso/pandoc.html"

  let ftype = over (content'type . args) (M.insert "type" ("\"" ++ show HTML ++ "\""))

  alternative <- newMultipart Alternative
  related <- ftype <$> newMultipart Related
  mixed <- ftype <$> newMultipart Mixed
  
  let readerExts = Set.union (Set.fromList [Ext_emoji]) $ readerExtensions def
      readerOpts = (def {readerStandalone = True,
                         readerExtensions = readerExts})
      writerOpts = (def {writerHighlight = True,
                         writerHighlightStyle = tango,
                         writerTemplate = Just template})
      compileHtml = writeHtmlString writerOpts . (\(Right e) -> e) . readMarkdown readerOpts
  
  let fhtml = encode . compileHtml . (++"</div>") . ("<div>"++)
  (Mail{_plain=plain,
       _html=html,
       _pj=pj}) <- over plain encode . over html fhtml <$> createMail ls
  
  return $ alternative {_elemlist=
    [Text (ContentType Plain M.empty) plain
    ,mixed {_elemlist=
            (related {_elemlist=
                     (Text (defaultContentType {_CT=HTML}) html)
                     : (map PJE . filter ((=="inline") . disposition) $ pj)})
            : (map PJE . filter ((=="attachment") . disposition) $ pj)}]}

  -- putStrLn $ createAlternative mail

