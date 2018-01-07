{-# LANGUAGE TemplateHaskell, DuplicateRecordFields #-}
module Lib where

-----------------------------IMPORTS---------------------------------
import QuotedPrintable
import Smiley
import List
import General
import Hunix (exec)
import ContentType
import qualified Html as H

import qualified Data.Set as Set
import Control.Monad
import Control.Lens hiding (element)
import Text.Regex.PCRE
import System.Process
import System.IO
import System.Directory
import Control.Monad
import Data.List
import System.Random (randomIO)
import Data.Map.Strict (Map, (!))
import qualified Data.Map.Strict as M

import Text.Pandoc hiding (Plain)
import Text.Pandoc.Readers.Markdown 
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Walk
import Skylighting

import Network.HTTP.Conduit (simpleHttp)
import qualified Data.ByteString.Lazy.Char8 as L
--------------------------------------------------------------------

max'attachment'size = 10^6 -- ^6

fileSize :: String -> IO Integer
fileSize = (read <$>) . exec "stat" . (:["-c","%s"])

uploadFile = (last . lines <$>)
  . exec "scp_website_with_date" . (:[])

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

guessLanguage filename
  | f ".R .r" = "R"
  | f ".hs" = "Haskell"
  | f "c h" = "C"
  | f "cpp" = "C++"
  | f "py"  = "Python"
  | otherwise = ""
  where f = any (`endWith` filename) . words
  

-- create_attachment :: String -> IO (Maybe Mail)
create_attachment = g . trim
  where g path = doesFileExist path >>= (f ? return Nothing)
          where f = do
                  size <- fileSize path
                  mimeType <- getFileMimeType path
                  let category = head . splitOn (=='/') $ mimeType
                      filename = basename path
                      
                  if size > max'attachment'size
                    then uploadFile path >>=
                         \url -> return . Just $ mempty {_plain=url,
                                        _html=surround2 "[" "]" filename
                                              ++ surround2 "(" ")" url}
                    else do
                    b64 <- encode_base64 path
                    id <- randomID
                    let ans = mempty {_plain=plaina, _pj=[defaultPJ
                                       {oid=id,
                                        mimetype=defaultContentType{_CT=MIME mimeType},
                                        name=filename,
                                        content=b64,
                                        disposition=if category == "image"
                                                    then "inline" else "attachment"}]}
                          where plaina = "["++mimeType++": "++filename++"@"++id++"]"
                        preview_image = show $ H.div [img]
                          where img = H.img
                                      . M.fromList
                                      $ [("src", "cid:"++id),
                                          ("alt", filename)]
                        preview_text = do
                          s <- readFile path
                          let f = H.div' ["scrollable black"] . (:[]) . H.Text
                                  . surround2 (surround "\n" ("~~~~"++guessLanguage filename)) "\n~~~~\n"
                          return $ surround "\n----\n"
                            . (surround "\n"
                                (surround "**" filename++":")++)
                            . show $ f s

                    Just <$> case category of
                               "image" -> return ans{_html=preview_image}
                               "text" -> preview_text >>= \s ->
                                                            return ans{_html=s}
                               _ -> return ans
                                               
                -- g categ = do
 
                  
                  -- let 
                      
                  --     pj = 
                  --     makeAns "image" = return $ ans {
                  --       _html=html
                  --       ,_pj=[pj{disposition="inline"}]}
                  --       where html = show $ H.div [img]
                  --               where img = H.img
                  --                       . M.fromList
                  --                       $ [("src", "cid:"++id),
                  --                          ("alt", filename)]
                  --                     title = H.tag "p" [H.Text filename]
                      
                  --     makeAns "text" = do
                  --       s <- readFile path
                  --       let f = H.div' ["scrollable black"] . (:[]) . H.Text
                  --             . surround2 (surround "\n" ("~~~~"++guessLanguage filename)) "\n~~~~\n"
                  --       return $ ans {_html=surround "\n----\n"
                  --                           . (surround "\n"
                  --                               (surround "**" filename++":")++)
                  --                           . show $ f s,
                  --                     _pj=[pj{disposition="attachment"}]}
                  --     makeAns _ = return $ ans {_pj = [pj{disposition="attachment"}]}
                        
                  -- Just <$> if size < max'attachment'size
                  --          then makeAns category
                  --          else uploadFile path >>=
                  --               \s -> return $ mempty {_plain=s,
                  --                                      _html=s}
                  --   -- then ans {_html = show html
                  --   --          ,_pj = [pj{disposition="inline"}]}
                  --   -- else ans {_pj = [pj{disposition="attachment"}]}


create_link_preview l = undefined -- simpleHttp l >>= 
  

-- createMail :: [String] -> IO Mail
createMail ls = foldM f mempty ls
  where f mail l@('/':_) = (mappend mail . just_or_default (default_line l))
          <$> create_attachment (trim l)
        -- f mail l | beginWith "@link:" l = create_link_preview $ drop 5 l
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

compileHtml :: String -> String -> String
compileHtml template html =
  let readerExts = Set.union (Set.fromList [Ext_emoji]) $ readerExtensions def
      readerOpts = (def {readerStandalone = True,
                         readerExtensions = readerExts})
      writerOpts = (def {writerHighlight = True,
                         writerHighlightStyle = tango,
                         writerTemplate = Just template})
  in writeHtmlString writerOpts . (\(Right e) -> e) . readMarkdown readerOpts $ html

runMail :: String -> IO Mail
runMail str = do
  ls <- lines <$> substitute_smileys str
  template <- readFile "/home/mika/.perso/pandoc.html"
  over html (compileHtml template) <$> createMail ls

run str = do

  let ftype = over (content'type . args) (M.insert "type" ("\"" ++ show HTML ++ "\""))
  alternative <- newMultipart Alternative
  related <- ftype <$> newMultipart Related
  mixed <- ftype <$> newMultipart Mixed
  
  (Mail{_plain=plain,
       _html=html,
       _pj=pj}) <- over plain encode . over html encode <$> runMail str
  
  return $ alternative {_elemlist=
    [Text (ContentType Plain M.empty) plain
    ,mixed {_elemlist=
            (related {_elemlist=
                     (Text (defaultContentType {_CT=HTML}) html)
                     : (map PJE . filter ((=="inline") . disposition) $ pj)})
            : (map PJE . filter ((=="attachment") . disposition) $ pj)}]}

