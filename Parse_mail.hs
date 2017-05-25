module Parse_mail where
-- module Parse_mail (parseHeaders,  selectHeader, create_answer_mail, Mail (..)) where

import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Ch8 (pack, unpack)
import qualified Codec.MIME.QuotedPrintable as QP (encode, decode)

import Data.List
import List hiding (toLower)
import System.IO
import Text.Regex.PCRE

-- 
type Mail_header = [String]

data Mail_content = MC { getH :: [Mail_header],
                         getContent :: [Mail_content]
                       }
                    | Mail_Leaf [String]

instance Show Mail_content where
  show (MC { getH = h, getContent = b}) = (unlines . (++[]) . map (notrailingln . unlines) $ h) ++ (unlines . map show) b
  show (Mail_Leaf l) = unlines l

isMultipartDeclaration = isInfixOf "content-type: multipart" . map toLower

findHeader :: String -> [[String]] -> Maybe [String]
-- findHeader _ _ = Nothing
-- findHeader _ [[]] = Nothing
findHeader h l  = safe_head . grp $ l
  where grp = filter (and . ([(>0).count, isInfixOf h . head]<*>) . pure) 
        


readMail :: [String] -> Mail_content
readMail l = MC {getH = h,
                 getContent = b}
   where (h',b') = sliceOn [] l
         h = readHeaders h'
         multih = findHeader "content-type: multipart" h
         b = case multih of (Just []) -> error "wtf"
                            (Just mh) -> readMultipartContent (head mh) b'
                            Nothing -> [Mail_Leaf b']

-- todo use fold
readHeaders :: [String] -> [Mail_header] -- a.k.a. [[String]]
readHeaders = spansplit isHeader

-- readHeaders :: [String] -> [Mail_header] -- a.k.a. [[String]]
-- readHeaders = rh []
--   where rh acc [] = acc
--         rh [] (l:ls) = rh [[l]] ls
--         rh acc@(a:as) (l:ls) | isHeader l = rh (acc++[[l]]) ls
--                              | otherwise  = rh ((a++[l]):as) ls


splitHeader = safe_sliceOn ':'

isHeader :: String -> Bool
-- isHeader = and . ([(/=Nothing),()] . pure) . sa
isHeader = (/=Nothing) . splitHeader
          
readMultipartContent :: String -> [String] -> [Mail_content]
readMultipartContent declaration content = map readMail $ readMultiparts content
  where Just (content_type,boundary) = readMultipartDeclaration declaration
        readMultiparts = map (filter (/=closeboundary boundary)) . splitOn (==boundary)

openboundary = ("--"++)
closeboundary = (++"--") . openboundary

readMultipartDeclaration :: String -> Maybe (String, String) -- type, boundary
readMultipartDeclaration = r . prepare
  where prepare = words . map toLower . replace '=' ' '
        r ("content-type:":c:"boundary":b:[]) = Just (c,filter (/='"') b)
        r _ = Nothing
                      


-- To: "Somebody" <somebody@email.com>                                          : main headers
-- From: mikaeldusenne@gmail.com
-- In-Reply-To: <0.0.4F.BC.1D17FA5B8EFC56C.0@omp.email.com>
-- References: <0.0.4F.BC.1D17FA5B8EFC56C.0@omp.email.com>
-- Subject: Réponse: Some stuff
-- Content-Type: multipart/related; boundary=relatedBoundary7894561230          : multipart1 declaration
--                                                                              : end of the headers (blank line)
-- --relatedBoundary7894561230                                                  : multipart1 boundary
-- Content-Type: multipart/alternative; boundary=alternativeboundary1234567890  : multipart1's header = multi2 declaration 
--                                                                              : end of the headers (blank line)
-- --alternativeboundary1234567890                            
-- Content-Type: text/plain; charset=UTF-8
-- Content-Transfer-Encoding: quoted-printable

-- --alternativeboundary1234567890
-- Content-Type: text/html; charset=UTF-8

-- <!DOCTYPE html><html><head>
-- <meta charset="UTF-8"></head><body></body></html>

-- --alternativeboundary1234567890--


-- --relatedBoundary7894561230

-- --relatedBoundary7894561230--

---- utils ----

toLowerStr = map toLower


-- take the headers of a mail
-- i.e. take the lines until we see a line starting with "content-type:"
takeHeaders = takeWhile (not . isPrefixOf "content-type: multipart/alternative" . map toLower ) 

takeBody :: [String] -> [String]
takeBody = filter (not . lineIsHeader)
           . takeWhile (f "")
           . tail
           . dropWhile (f "text/plain") --(f "text/plain")
  where f s = not . isPrefixOf ("content-type: "++s) . map toLower

-- check if line defines a new header, aka =~ ^[a-Z\-]:.*$
-- lineIsHeader l = any ((flip isPrefixOf).map toLower$l) mail_headers
lineIsHeader = (=~"^[a-zA-Z0-9\\-]+:") 

-- parseHeaders l = map (cutHeaders []) $ groupHeaders l []
--   where groupHeaders [] acc = acc
--         groupHeaders ("":_) acc = acc
--         groupHeaders (l:ls) [] = groupHeaders ls [l]
--         groupHeaders (l:ls) acc
--           | lineIsHeader l = groupHeaders ls $ l:acc
--           | otherwise      = groupHeaders ls $ (head acc ++ l) : (tail acc)

--         cutHeaders acc "" = (acc,"")
--         cutHeaders acc (':':' ':xs) = (acc,xs)
--         cutHeaders acc (':':xs) = (acc,xs)
--         cutHeaders acc (x:xs)  = cutHeaders (acc++[x]) xs

-- parsePointyBrackets = f []
--   where f acc [] = acc
--         f acc ('<':ls) = f (("<" ++ a ++ ">"):acc) b
--           where (a,b) = sliceOn '>' ls
--         f acc (_:ls) = f acc ls

-- -- split comma-separated string -> [string]
-- parseComma = f [] []
--   where f acc [] [] = acc
--         f acc mail [] = f (acc ++ [mail]) "" ""
--         f acc mail (',':xs) = f (acc ++ [mail]) "" xs
--         f acc mail ('"':xs) = f acc (mail ++ ('"':a++['"'])) b
--           where (a,b) = taketoquote [] xs
--                 taketoquote acc [] = (acc,"")
--                 taketoquote acc ('"':xs) = (acc,xs)
--                 taketoquote acc (x:xs) = taketoquote (acc ++ [x]) xs
--         f acc mail (x:xs) = f acc (mail++[x]) xs

-- selectHeader :: String -> [(String,String)] -> String
-- selectHeader h = f . g
--   where f Nothing = ""
--         f (Just (a,b)) = b
--         g = safe_head . filter ((==lowh) . toLowerStr . fst )
--         lowh = toLowerStr h

-- data Mail = Mail { getHeaders :: String ,
--                    getBody :: ByteString
--                  }
-- --             deriving Show

-- instance Show Mail where
--   show (Mail {getHeaders = h, getBody = b}) = "\n======== HEADER ========\n"
--                                               ++ h
--                                               ++ "\n======== BODY ========\n"
--                                               ++ Ch8.unpack b

-- create_answer_mail :: String -> Mail
-- create_answer_mail = genMail . lines 
--   where genMail s = Mail {getHeaders = genHeaders . takeHeaders $ s,getBody = genBody s}
--         genBody = Ch8.pack . QP.decode . unlines . map ('>':) . takeBody
--         genHeaders s =
--           let parsedHeaders = parseHeaders s
--               parse_pointy h = parsePointyBrackets . selectHeader h $ parsedHeaders
--               parse_comma h = parseComma . selectHeader h $ parsedHeaders
--               to = [selectHeader "from" parsedHeaders]
--               original_to_cc = parse_comma "to" ++ parse_comma "cc"
--               cc = filter (not . isInfixOf "mikaeldusenne") $ original_to_cc
--               from = filter (isInfixOf "mikaeldusenne") $ original_to_cc
              
--               subject = (" Réponse: "++) $ selectHeader "subject" parsedHeaders
                          
--               inreplyto = [selectHeader "message-id" parsedHeaders]
--               references = [selectHeader "references" parsedHeaders] ++ inreplyto
--           in unlines
--              . zipWith (++) ["To:","Cc:","From:","In-Reply-To:","References:","Subject:"]
--              . (++ [subject])
--              . (++(map concat [references])) . map (concatWith (","))
--              $ [to,cc,from,inreplyto]


main = do
  l <- lines <$> readFile "mail_original.txt"
  putStrLn . show $ readMail l
--   -- putStrLn $ parseComma " \"GILLIBERT, Andre\" <Andre.Gillibert@chu-rouen.fr>, mikaeldusenne@gmail.com"
  
--   -- mail_headers <- lines.(map toLower) <$> readFile "mail_headers.txt"
--   s <- takeHeaders.lines <$> getContents

--   -- writeFile "body" . unlines . takeBody $ s

--   hPutStrLn stderr . unlines $ s
--   hPutStrLn stderr "-----------------"
--   let parsedHeaders = parseHeaders s
--       parse_pointy h = parsePointyBrackets . selectHeader h $ parsedHeaders
--       parse_comma h = parseComma . selectHeader h $ parsedHeaders
--   -- let areHeaders = map lineIsHeader $ s 
--   -- hPutStrLn stderr $ unlines $ zipWith (\a -> \b -> show a ++ "\t" ++ show b) (map lineIsHeader s) s

--   -- putStrLn . unlines . map ((++"\n").show) $ parseHeaders s
           
-- --  putStrLn . show . parsePointyBrackets $ selectHeader "cc" parsedHeaders
--   let to = [selectHeader "from" parsedHeaders]
--       original_to_cc = parse_comma "to" ++ parse_comma "cc"
--       cc = filter (not . isInfixOf "mikaeldusenne") $ original_to_cc
--       from = filter (isInfixOf "mikaeldusenne") $ original_to_cc

--       subject = (" Réponse: "++) $ selectHeader "subject" parsedHeaders
      
--       inreplyto = [selectHeader "message-id" parsedHeaders]
--       references = [selectHeader "references" parsedHeaders] ++ inreplyto
-- --  -- debug
-- --  print "(((((((("
-- --  print $ parseHeaders ["To: <Mikaeldusenne>","In-Reply-To: someone" ]
-- --  print "))))))))"
-- --  hPutStrLn stderr $ show original_to_cc
--   putStr . unlines . zipWith (++) ["To:","Cc:","From:","In-Reply-To:","References:","Subject:"]
--     . (++ [subject])
--     . (++(map concat [references])) . map (concatWith (",")) $ [to,cc,from,inreplyto]

