module MailRedactor.Attachment where

import qualified Data.Map.Strict as M
import System.Directory
import System.FilePath.Posix

import qualified Html as H
import MailRedactor.Types
import MailRedactor.ContentType
import MailRedactor.Utils
import List
import Misc
import Hunix
import CSV

max'attachment'size = 5 * 10^6 -- ^6
max'text'preview'lines = 500


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
