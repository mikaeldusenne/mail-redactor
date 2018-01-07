{-# LANGUAGE TemplateHaskell, DuplicateRecordFields #-}
module ContentType where

import Control.Lens

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

data CT = Alternative | Mixed | Related | Plain | HTML | MIME String

data ContentType = ContentType {_CT :: CT,
                                _args :: Map String String}
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

defaultContentType = ContentType (MIME "") M.empty
