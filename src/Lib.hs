{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE TemplateHaskell, DuplicateRecordFields #-}
module Lib where

-----------------------------IMPORTS---------------------------------
import QuotedPrintable
import Smiley
import List
import General
import Hunix
import ContentType
import qualified Html as H

import Data.List
import Data.Monoid
import qualified Data.Text as T (pack, unpack, Text)
import Control.Monad
import Control.Lens hiding (element)
import System.Directory
-- import Text.Regex.PCRE
-- import System.Process
-- import System.IO
-- import Control.Monad
-- import Data.List
import qualified Data.Map.Strict as M
-- import Text.Pandoc.Walk
-- import Network.HTTP.Conduit (simpleHttp)
-- import qualified Data.ByteString.Lazy.Char8 as L

-- import qualified Codec.Binary.Base64.String as Base64
-- import qualified Data.ByteString.Base64.Lazy as Base64
-- import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.UTF8 as BSUTF8
import qualified Data.ByteString.Base64 as Base64Strict
import System.FilePath.Posix

import Text.Pandoc.Extensions
import Text.Pandoc.Readers.Markdown 
import Text.Pandoc.Writers.HTML
import System.Random (randomIO)
import qualified Data.Map.Strict as M

import Text.Pandoc hiding (Plain)
import Skylighting
import CSV
--------------------------------------------------------------------

max'attachment'size = 5 * 10^6 -- ^6
max'text'preview'size = 5 * 10^4
max'text'preview'lines = 500

randomID :: IO [Char]
randomID = (("yolo"++) . show . abs) <$> (randomIO :: IO Integer)

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
  show (PJ {oid=i, mimetype=m, disposition=d, name=n, content=b64}) = unlines [
    show m ++"; name=\"" ++ n ++ "\""
    ,"Content-Disposition: "++ d ++"; filename=\""++ n ++"\""
    ,show B64
    ,"Content-ID: <" ++ i ++ ">"
    ,"X-Attachment-Id: " ++ i
    ,"\n" ++ b64]

defaultPJ :: PJ
defaultPJ = PJ{oid="", mimetype=ContentType (MIME "") M.empty, disposition="", name="", content=""} :: PJ

extensionLanguage :: [([Char], [[Char]])]
extensionLanguage = [("r",         ["R", "r"]),
                     ("haskell",   ["hs"]),
                     ("c",         ["c", "h"]),
                     ("c++",       ["c++", "cpp"]),
                     ("python",    ["py"]),
                     ("latex",     ["tex", "latex"]),
                     ("sh",        ["sh"]),
                     ("html",      ["html"]),
                     ("javascript",["js"]),
                     ("csv",       ["csv"])]

guessLanguage :: String -> Maybe String
guessLanguage filename = search . drop 1 . takeExtension $ filename
  where search ext = (fst <$>) . safe_head . filter (any (==ext) . snd) $ extensionLanguage

-- highlighting custom styles: https://stackoverflow.com/questions/30880200/pandoc-what-are-the-available-syntax-highlighters

-- create_attachment :: String -> IO (Maybe Mail)
create_attachment :: [Char] -> IO (Maybe Mail)
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
                    i <- randomID
                    let ans = mempty {_plain=plaina, _pj=[defaultPJ
                                       {oid=i,
                                        mimetype=defaultContentType{_CT=MIME mimeType},
                                        name=filename,
                                        content=b64,
                                        disposition=if category == "image"
                                                    then "inline" else "attachment"}]}
                          where plaina = "["++mimeType++": "++filename++"@"++i++"]"
                        preview_image = show $ H.div [img]
                          where img = H.img
                                      . M.fromList
                                      $ [("src", "cid:"++i),
                                         ("alt", filename)]
                        preview_text = do
                          s <- readFile path
                          let surroundPreview =
                                let filetype = guessLanguage filename
                                in case filetype of
                                     Just "csv" -> \s -> let sep = guessSep s
                                       in case sep of Nothing -> ""
                                                      Just c -> let csv = parseCSV c s
                                                        in surround "\n" $ show $ H.div' ["scrollable"] [H.table csv]
                                     ft -> surround2 (surround "\n" ("```{"
                                                                     ++(just_or_default "" $ (" ."++) <$> ft)
                                                                     ++" .scrollable"
                                                                     ++" }")) "\n```\n"
                          if length (lines s) > max'text'preview'lines
                            then return ""
                            else return $ surround "\n----\n"
                                 . (surround "\n"
                                    (surround "**" filename++":")++)
                                 -- . show
                                 $ surroundPreview s

                    Just <$> case category of
                               "image" -> return ans{_html=preview_image}
                               "text" -> preview_text >>= \s ->
                                                            return ans{_html=s}
                               _ -> return ans

-- createMail :: [String] -> IO Mail
createMail :: Foldable t => t [Char] -> IO Mail
createMail ls = foldM f mempty ls
  where f mail l@('/':_) = (mappend mail . just_or_default (default_line l))
                           <$> create_attachment (trim l)
        f mail l = return . mappend mail $ default_line l
        default_line l = mempty{_html=l', _plain=l'}
          where l' = l ++ "  "

data Element = Multipart {_content'type :: ContentType
                          ,_elemlist :: [Element]}
             | Text { _content'type :: ContentType,
                      _s ::  String }
             | PJE PJ
makeLenses ''Element

instance Show Element where
  show (PJE e) = show e
  show (Text {_content'type=t, _s=str}) = unlines
    [show (if (_CT t)==HTML then B64 else QP)
     -- show QP
    ,show . over args (M.insert "charset" "utf-8") $ t
    ,"", str]
  show (Multipart {_content'type=t@(ContentType _ m),
                   _elemlist=l}) = unlines
    [show t,""
    ,unlines . insertBeforeEach ("--"++b) . map show $ l
    ,"", "--"++b++"--"]
    where b = m M.! "boundary"

newMultipart :: CT -> IO Element
newMultipart t = randomID >>= \str ->
  let m = M.fromList [("boundary",ctStr t ++ str)]
  in return Multipart{ _content'type=ContentType t m,
                       _elemlist=[]}

-- compileHtml :: String -> String -> String
compileHtml :: PandocMonad m => [Char] -> String -> m T.Text
compileHtml template html =
  let -- readerExts = Set.union (Set.fromList [Ext_emoji]) $ readerExtensions def
      readerOpts = (def {readerStandalone = True,
                         readerExtensions = githubMarkdownExtensions <> extensionsFromList [Ext_emoji, Ext_raw_html, Ext_fenced_code_blocks, Ext_fenced_code_attributes]})
      writerOpts = (def {writerHighlightStyle = Just tango,
                         writerTemplate = Just template})
      a :: Pandoc
      a = fromEither . runPure $ (readMarkdown readerOpts . T.pack $ html)
  in  writeHtml5String writerOpts a

runMail :: String -> IO Mail
runMail str = do
  ls <- lines <$> substitute_smileys str
  template <- readFile "/home/mika/.perso/pandoc.html"
  over html (T.unpack . fromEither . runPure <$> compileHtml template) <$> createMail ls

run :: String -> IO Element
run str = do
  let ftype = over (content'type . args) (M.insert "type" ("\"" ++ show HTML ++ "\""))
      onlyPlain = return
                  . Text (defaultUTF8ContentType {_CT=Plain})
                  . encode
                  $ (unlines.tail.lines) str
        
      alsoHTML = do
        alternative <- newMultipart Alternative
        related <- ftype <$> newMultipart Related
        mixed <- ftype <$> newMultipart Mixed
  
        (Mail{_plain=plaintext,
              _html=htmltext,
              _pj=pjmail}) <- over plain encode
                              . over html (BSUTF8.toString
                                           . Base64Strict.encode
                                           . BSUTF8.fromString)
                              <$> runMail str

        -- b46_html <- encode_
  
        return $ alternative {_elemlist=
                              [Text (defaultUTF8ContentType {_CT=Plain}) plaintext
                              ,mixed {_elemlist=
                                      (related {_elemlist=
                                                (Text (defaultUTF8ContentType {_CT=HTML}) htmltext)
                                                : (map PJE . filter ((=="inline") . disposition) $ pjmail)})
                                      : (map PJE . filter ((=="attachment") . disposition) $ pjmail)}]}

          
  if (head $ lines str) == "!plain!" then onlyPlain else alsoHTML
