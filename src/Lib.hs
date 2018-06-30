{-# LANGUAGE TemplateHaskell,LambdaCase #-}

module Lib where
-----------------------------IMPORTS---------------------------------
import qualified Data.Text as T (pack, unpack, Text)
import QuotedPrintable
import Smiley
import List
import Misc
import Hunix
import MailRedactor.ContentType
import MailRedactor.Utils
import Control.Lens hiding (element)

import Data.List
import Control.Monad
import System.Directory

import qualified Data.Map.Strict as M
import qualified Data.ByteString.UTF8 as BSUTF8
import qualified Data.ByteString.Base64 as Base64Strict
import System.FilePath.Posix

import Data.String
import Data.FileEmbed

import MailRedactor.Types
import MailRedactor.Attachment
import MailRedactor.Html
--------------------------------------------------------------------

-- createMail :: [String] -> IO Mail
createMail :: Foldable t => t [Char] -> IO Mail
createMail ls = foldM f mempty ls
  where f mail l@('/':_) = (mappend mail . just_or_default (default_line l))
                           <$> create_attachment (trim l)
        f mail l = return . mappend mail $ default_line l
        default_line l = mempty{_html=l', _plain=l'}
          where l' = l ++ "  "

runMail :: String -> IO Mail
runMail str = do
  -- ls <- lines <$> substitute_smileys str
  ls <- mapM (\case l@('|':xs) -> return l
                    l -> substitute_smileys l) $ lines str
  let template = $(embedStringFile "data/pandoc.html")
  over html (T.unpack . fromEither <$> compileHtml template) <$> createMail ls

run :: Bool -> String -> IO Element
run debug str = do
  let ftype = over (content'type . args) (M.insert "type" ("\"" ++ show HTML ++ "\""))
      onlyPlain = return
                  . Text (defaultUTF8ContentType {_CT=Plain})
                  . encode
                  $ (unlines.tail.lines) str
        
      alsoHTML = do
        alternative <- newMultipart Alternative
        related <- ftype <$> newMultipart Related
        mixed <- ftype <$> newMultipart Mixed

        mail <- runMail str
        
        let (Mail{_plain=plaintext,
                  _html=htmltext,
                  _pj=pjmail}) = over plain encode
                                 . over html (BSUTF8.toString
                                              . Base64Strict.encode
                                              . BSUTF8.fromString)
                                 $ mail

        -- b46_html <- encode_
        when debug $ print $ _html mail
        return $ alternative {_elemlist=
                              [Text (defaultUTF8ContentType {_CT=Plain}) plaintext
                              ,mixed {_elemlist=
                                      (related {_elemlist=
                                                (Text (defaultUTF8ContentType {_CT=HTML}) htmltext)
                                                : (map PJE . filter ((=="inline") . disposition) $ pjmail)})
                                      : (map PJE . filter ((=="attachment") . disposition) $ pjmail)}]}

          
  if (head $ lines str) == "!plain!" then onlyPlain else alsoHTML
