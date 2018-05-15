{-# LANGUAGE TemplateHaskell, DuplicateRecordFields #-}
module MailRedactor.Types where

import qualified Data.Map.Strict as M
import Control.Lens hiding (element)
import MailRedactor.ContentType
import MailRedactor.Utils
import List
import Control.Lens


data CT = Alternative | Mixed | Related | Plain | HTML | MIME String
  deriving(Eq)

data ContentType = ContentType {_CT :: CT,
                                _args :: M.Map String String}
makeLenses ''ContentType

instance Show CT where
  show Alternative = "multipart/alternative"
  show Mixed       = "multipart/mixed"
  show Related     = "multipart/related"
  show Plain       = "text/plain"
  show HTML        = "text/html"
  show (MIME s)    = s

ctStr Alternative = "alternative"
ctStr Mixed       = "mixed"
ctStr Related     = "related"
ctStr Plain       = "plain"
ctStr HTML        = "html"
ctStr (MIME s)    = s


instance Show ContentType where
  show (ContentType ct l) =
    "Content-Type: " ++ show ct ++ concat (map f . M.toList $ l)
    where f (k, v) = "; " ++ k ++ "=" ++ v

defaultContentType = ContentType (MIME "") mempty
defaultUTF8ContentType = ContentType (MIME "") (M.fromList [("charset", "utf-8")])



data TransferEncoding = B64 | QP

-- attachment
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

defaultPJ = PJ{oid="", mimetype=ContentType (MIME "") mempty, disposition="", name="", content=""}


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
