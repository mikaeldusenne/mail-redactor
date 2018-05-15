module MailRedactor.Html where

import qualified Data.Text as T (pack, unpack, Text)
import Data.Monoid

import Text.Pandoc.Readers.Markdown 
import Text.Pandoc.Extensions
import Text.Pandoc.Writers.HTML
import Text.Pandoc hiding (Plain)
import Skylighting

import Misc

import MailRedactor.Utils

compileHtml :: [Char] -> String -> Either PandocError T.Text
compileHtml template html =
  let -- readerExts = Set.union (Set.fromList [Ext_emoji]) $ readerExtensions def
      readerOpts = (def {readerStandalone = True,
                         readerExtensions = githubMarkdownExtensions <> extensionsFromList [Ext_emoji, Ext_raw_html, Ext_fenced_code_blocks, Ext_fenced_code_attributes]})
      writerOpts = (def {writerHighlightStyle = Just tango,
                         writerTemplate = Just template})
      a :: Pandoc
      a = fromEither . runPure $ (readMarkdown readerOpts . T.pack $ html)
  in runPure $  writeHtml5String writerOpts a
