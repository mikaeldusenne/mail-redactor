module Parse_mail where
-- module Parse_mail (parseHeaders,  selectHeader, create_answer_mail, Mail (..)) where

import Data.Char
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as Ch8 (pack, unpack)
import qualified Codec.MIME.QuotedPrintable as QP (encode, decode)

import Data.List

import List hiding (toLower)
import Tuple

import System.IO
import Text.Regex.PCRE

-- 



-- data Mail = Mail { getH :: [Mail_header],
--                    getContent :: [Mail]
--                  }
--           | Mail_Leaf [String]
--           | DummyMail

data Mail = Mail { getH :: [Mail_header],
                   getContent :: Mail_body}

type Mail_header = (String, String) -- title, content -- TODO as a proper data

data Mail_body = Mail_Leaf [String]
               | Mail_multipart { getBoundary :: String,
                                  getParts    :: [Mail]}
               | DummyMail

show_header h = fst h ++ ": " ++ snd h

-- todo rm notrailingln??
show_headers = unlines . (++[""]) . map (notrailingln . show_header)

instance Show Mail where
  show (Mail { getH = h, getContent = b}) = show_headers h ++ show b

instance Show Mail_body where
  show (Mail_Leaf l) = unlines l
  show DummyMail     = "<Dummy Mail>"
  show (Mail_multipart {getBoundary = b, getParts = parts}) = unlines $
    beforeEach (openboundary b) (map show parts) ++ [closeboundary b]
  
-- isMultipartDeclaration = isInfixOf "content-type: multipart" . map toLower
isMultipartDeclaration (ht,hv) = (&&) (isInfixOf "content-type" ht) (isInfixOf "multipart" hv)

-- from a list of headers, find the first one beginning with <String>
findHeader :: String -> [Mail_header] -> Maybe Mail_header
-- findHeader h l  = safe_head . grp $ l
--   where grp = filter (and . ([(>0).count, isInfixOf h . head]<*>) . pure) 
findHeader h l = fst <$> separateHeader h l

-- header_title_corresponds s = isPrefixOf s . fst
header_value_correspond s = isPrefixOf (sanitize s) . sanitize 
header_corresponds (st,sv) (ht, hv) = mail_header_title_correspond st ht && header_value_correspond sv hv
-- header_corresponds t1 t2 = uncurry (&&) $ zipTuplesWith mail_header_title_correspond t1 t2

-- find a header title <h> in a list of headers and returns ( <found header> , <rest of header list> )
separateHeader h = separate ((==h) . map toLower . fst)
  
readMail :: [String] -> Mail
readMail l = Mail {getH = h,
                   getContent = b}
  where (h',b') = sliceOn [] l
        h = readHeaders h'
        multih = fst <$> separate (header_corresponds ("content-type","multipart")) h
        ct = Nothing
        cte = findHeader "content-transfer-encoding"
        b = case multih of (Just mh) -> readMultipartContent mh b'
                           Nothing -> Mail_Leaf b'

readMultipartContent :: Mail_header -> [String] -> Mail_body
readMultipartContent declaration content = Mail_multipart {getBoundary = boundary ,
                                                           getParts = map readMail . readMultiparts $ content}
  where Just (content_type,boundary) = readMultipartDeclaration declaration
        readMultiparts = map (filter (/=closeboundary boundary)) . splitOn (==openboundary boundary)

readMultipartDeclaration :: Mail_header -> Maybe (String, String) -- type, boundary
readMultipartDeclaration = r . prepare . snd
  where prepare = words . replace ';' ' ' . replace '=' ' '
        r (c:boundary:b:[]) | map toLower boundary == "boundary" = Just (c,filter (/='"') b)
                            | otherwise = Nothing
        r e = Nothing --error $ "failed to parse "++(show e)

readHeaders :: [String] -> [Mail_header]
readHeaders = map (\(Just e) -> e) . filter (/=Nothing) . map readHeader . spansplit isHeader

readHeader :: [String] -> Maybe Mail_header
readHeader hl = (\(a,b) -> (a,tail b)) <$> (safe_sliceOn ':' . unlines $ hl)

splitHeader = safe_sliceOn ':'

isHeader :: String -> Bool
isHeader = (/=Nothing) . splitHeader

openboundary = ("--"++)
closeboundary = (++"--") . openboundary




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

mail_header_title_correspond t1 t2 = allSame . map sanitize $ [t1,t2]

sanitize = map toLower . trim

trim = trimBeg . reverse . trimBeg . reverse
  where trimBeg = dropWhile isSpace
        

-- todo lenses?
changeHeader :: String -> String -> [Mail_header] -> [Mail_header]
changeHeader title value [] = []
changeHeader title value (h@(ht,hv):hs)
  | mail_header_title_correspond title ht = (ht,value):hs
  | otherwise   = (h:) $ changeHeader title value hs


answer_to_mail :: Mail -> Mail
answer_to_mail (Mail { getH = h, getContent = b }) = Mail { getH = newheaders, getContent = b' }
  where getval (Just e) = snd e
        findval = getval . (flip findHeader) h
        oldfrom = findval "from"
        oldto = findval "to"
        newheaders = [("from",oldto),("to",oldfrom)]

        map duplicate ["cc",""]
        [("from","to"),("to","from")]
        
        -- h'' = changeHeader "from" . getval . findHeader "to" $ h
        -- h' = changeHeader "to" oldfrom h
        b' = b
        
        

main = do
  l <- lines <$> readFile "mail_gmail_fabien_2"
  putStrLn . unlines $ l
  putStrLn "\n\n"++take 29 (repeat "§")++"\n"
  let m@Mail{getH = h, getContent = c} = answer_to_mail $ readMail l
      
  putStrLn . show $ m
  print "ok"
  
