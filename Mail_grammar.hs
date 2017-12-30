module Mail_grammar where
import List 
import Tuple
import Data.Char

surroundWith s = (s++) . (++s)
surroundcomment = surroundWith . ('(':) . (++")")

crlf = map chr [13, 10]
alpha = ['a'..'z'] ++ ['A'..'Z']
num = ['0'..'9']
alphanum = alpha ++ num
wsp = " \r\n"


atom_charset = alphanum ++ "!#$%&'*+-/=?^_`{|}~"
specials_charset = "()<>[]:;@\\,.\""
quoted_charset = map chr . except [34,92] $ [33..126] -- printable US-ASCII except backslash and quote

data Quoted = Q Char

data Atom = Atom String
data Dot_atom = Dot_atom String

data Quoted_string = QS String

-- todo read_word read_phrase read_unstructured ?
-- data Address = MailBox | Group
-- data MailBox = Name_address | Address_spec
-- data Name_address = [Display_name] Angle_Address
-- data Angle_Address = Angle Address_spec
-- data Group = Display_name ":" [Group_list] ";"
-- data Display_name = Phrase
-- data Mailbox_list = (mailbox *("," mailbox)) / obs-mbox-list
-- data Address_list = (address *("," address)) / obs-addr-list
-- data Group_list =  mailbox-list / CFWS / obs-group-list

data Name_address = Name_address { name_address'name :: String,
                                   name_address'address :: Address}
-- data Angle_Address = Angle Address
data Address = At Local_part Domain
type Local_part = Dot_atom
type Domain = Dot_atom

data Mailbox = MN Name_address | MA Address

data MailboxList = MBL [Mailbox]

data Mail = Mail{ mail'headers :: Headers,
                  mail'body :: Body}

type Headers = [Header]
data Body = Body String

data Header = Header{ header'name::Header_name,
                      header'body::Header_body}

data Header_name = HN String
data Header_body = HB_mailbox Mailbox | HB_mailboxlist MailboxList | HB_unstructured String

-- SHOW

instance Show Quoted where
  show (Q c) = c:""

instance Show Atom where
  show (Atom s) = s

instance Show Dot_atom where
  show (Dot_atom s) = s

instance Show Quoted_string where
  show (QS s) = '"': s ++ ('"':[])

instance Show Address where
  show (local_part`At`domain) = show local_part ++ "@" ++ show domain

instance Show Name_address where
  show (Name_address {name_address'name = n,
                      name_address'address = a}) =
    n ++ " <" ++ show a ++ ">"

-- instance Show Mailbox_list where
--   show (

instance Show Header where
  show (Header {header'name=n, header'body=b}) = surroundcomment "h" $ show n++": " ++ show b

instance Show Header_name where
  show (HN s) = s

instance Show Header_body where
  show (HB_unstructured s) = surroundcomment "~" s
  show (HB_mailbox s) = surroundcomment "." $ show s
  show (HB_mailboxlist s) = surroundcomment "|" $ show s
  
instance Show Mailbox where
  show (MA a) = show a
  show (MN a) = show a

instance Show MailboxList where
  show (MBL l) = concatWith ",\r\n\t" . map show $ l

-- READ
-- reads = readsPrec 0

trimwsp = dropWhile (`elem`wsp)
slicewsp = span (`elem`wsp)


readPhrase :: ReadS String
readPhrase r = [f [] r]
  where f acc [] = ret acc ""
        f acc s = case ra of [(Atom a,as)] -> f (acc++[a]) as
                             [] -> ret acc s
          where ra = readsPrec 0 s
        ret acc rest = (concatWith " " acc, rest)

instance Read Quoted where
  readsPrec _ ('\\':c:xs) = [(Q c,xs)]
  readsPrec _ _ = []

instance Read Atom where
  readsPrec k r = if isEmpty a then [] else [(Atom a,xs)]
    where (a,xs) = applyToFst trimwsp -- ?
            . span (`elem`atom_charset) . trimwsp $ r

instance Read Dot_atom where
  readsPrec k r = case atom of []           -> []
                               [(Atom a,s)] -> [readots a s]
    where atom = readsPrec (k+1) r :: [(Atom,String)]
          readots acc ('.':xs) = case readsPrec (k+1) xs
                                 of [(Atom a,xs')] -> readots (acc++"."++a) xs'
                                    [] -> error "wtf nooo"
                                    --[] -> (Dot_atom acc,xs)
          readots acc xs = (Dot_atom acc,xs)

instance Read Quoted_string where
  readsPrec k r = case s of [(q,xs)] -> [(QS q, xs)]
                            [] -> []
    where s = readsPrec (k+1) r
--   readsPrec k r = [(QS q, xs)]
--     where (q,xs) = f "" r
--           f acc "" = (acc,"")
--           f acc r = 

instance Read Address where
  readsPrec k r = [(local`At`domain, xs) |
                   (local@(Dot_atom _),'@':s) <- readsPrec (k+1) r,
                   (domain@(Dot_atom _),xs) <- readsPrec (k+1) s]

instance Read Name_address where
  readsPrec k r = [(Name_address n a,xs) |
                   (n,s) <- readPhrase r,
                   (a@(_`At`_),'>':xs) <- readsPrec (k+1) . cdr . trimwsp $ s ]
                  ++
                  [(Name_address (show n) a, xs) |
                   (QS n,s) <- readsPrec (k+1) r,
                   (a@(_`At`_),'>':xs) <- readsPrec (k+1) . cdr . trimwsp $ s]

instance Read Mailbox where
  readsPrec k r = [(MN a,xs) | (a,xs) <- readsPrec k r]
                  ++ [(MA a, xs) | (a, xs) <- readsPrec k r]
  -- readsPrec k r = concat $ map f [MN,MA]
  --   where f t = [(t a,xs) | (a,xs) <- readsPrec k r]

instance Read MailboxList where
  readsPrec k r = case ans of [] -> []
                              [(h@(a:as),xs)] -> [(MBL h,xs)]
    where ans = f [] r
          f acc s = case mb of [(a,s')] -> case c of ',' -> f (a:acc) s''
                                                     _   -> [(reverse (a:acc), s')]
                               [] -> []
            where mb = readsPrec (k+1) s
                  (c:s'') = trimwsp . snd . head $ mb
                  
-- instance Read MailboxList where
--   readsPrec k r = [(MBL l,xs)]
--     where (l,xs) = f [] r
--           f acc s = case c of ',' -> f (a:acc) s''
--                               _   -> (reverse (a:acc), s')
--             where [(a, s')] = readsPrec (k+1) s
--                   (c:s'') = trimwsp s'
                  



instance Read Header where
  readsPrec k r = [(Header {header'name=n, header'body=b},xs)
                  | (n,':':s) <- readsPrec (k+1) r,
                    (b,xs) <- readsPrec (k+1) s]

instance Read Header_name where
  readsPrec _ r = [(HN n, xs)]
    -- any from 33 to 126 except 58
    where headerCharset = map chr . except [58] $ [33..126]
          (n,xs) = span (`elem`headerCharset) r 

perhapsConstruct c e = case e of [] -> []
                                 [(a, xs)] -> [(c a, xs)]

isHeaderBeginning s = (not.isEmpty$hn) && ((==':').head$xs) 
  where hn = (reads :: ReadS Header_name) s
        [(_,xs)] = hn

instance Read Header_body where
  readsPrec _ r = if (not.isEmpty$hbtok) then hbtok else hb_unstructured
    where hbmb = (\[(mb,xs)] -> [(HB_mailbox mb,xs)]) . reads $ r
          hbmbl = perhapsConstruct HB_mailboxlist . reads $ r
          hbtok = hbmbl
          hb_unstructured = perhapsConstruct HB_unstructured . (:[]) . applyToTuple (concatWith "\r\n") . span (not.isHeaderBeginning)
            . splitWhen "\r\n" $ r
          -- hb_unstructured = (\(s,xs) -> [(HB_unstructured . (s++) $ hbu xs "",xs)]) . sliceCRLF $ r
          
          -- hbu s acc = if (isEmpty $ (reads :: ReadS Header_name)) && (not . isEmpty l)  s -- new header or end of headers
          --             then hbu ls (acc ++ "\r\n" ++ l)
          --             else (acc,s)
          --   where (l,ls) = sliceCRLF s
--    (b,xs) = sliceCRLF r

read_headers :: String -> [([Header],String)]
read_headers s = r s []
  where r [] acc = [(reverse acc,"")]
        r s acc = case r' of []           -> [(reverse acc, s)]
                             [(Header a b,s')] -> r s' ((Header a b):acc)
                             l@(x:xs) -> error $ "\n\nàààààààààà\n\n" ++ show x ++ "\n\nooo\n\n" ++ show xs
          where r' = (reads::ReadS Header) . trimwsp $ s

sliceCRLF = s ""
  where s acc "" = (reverse acc,"")
        s acc s@('\r':'\n':xs) = (reverse acc,xs)
        s acc (x:xs) = s (x:acc) xs

main = do
  -- let hn = HN "From"
  --     hb = HB "mikaeldusenne@gmail.com"
  --     h = Header hn hb
  --     h' = read "From: mikaeldusenne@gmail.com" :: Header
  -- putStrLn . show $ h'
  m <- readFile "mail_test_anne_gribouval_forheadertesr_bis.txt"
  print m
  putStr $ surroundWith "\n\n" "-------------------------------------------------------------"
  let (h,b) =  head $ read_headers m
      
  putStrLn . concatWith "\n\n====\n\n" . map show $ h
  putStrLn $ "\n\n++++++++\n\n" ++ b
  
