module Mail_headers where

import List
data Quoted_string = QS String

-- group box and domain names ?
data Box_name = Box_name String
data Domain_name = Domain_name String
data Mail_address = At Box_name Domain_name
data Bracketted_address = Bracketted Mail_address



data Mail_identity = Single_address Mail_address
                   | Target
                     {get'target_name :: Quoted_string,
                      get'target_address :: Bracketted_address}



type Header_name = String

-- data Addresses = Addresses [Mail_identity]

data Header_content = Header_single_address Mail_identity
                    | Address_list [Mail_identity]
                    | Header_string String

data Header = Header {get'header_name :: Header_name,
                      get'header_content :: Header_content}

addresslist_header :: Header_name -> Header_content -> Header
addresslist_header n al@(Address_list l) =
  Header {get'header_name = n,
          get'header_content = al}
addresslist_header n _ =
  error $ show n ++ "field must contain mail addresses"


from t@(Target _ _) =
  Header {get'header_name = "From",
          get'header_content = Header_single_address t}
from _ = error "From: must contain one target"

to :: Header_content -> Header
to = addresslist_header "To"

cc = addresslist_header "Cc"
bcc = addresslist_header "Bcc"

type Headers = [Header]


------------- Show

instance Show Quoted_string where
  show (QS s) = "\"" ++ s ++ "\""

instance Show Box_name where
  show (Box_name box) = box

instance Show Domain_name where
  show (Domain_name domain) = domain

instance Show Mail_address where
  show (At b d) = show b ++ "@" ++ show d

instance Show Bracketted_address where
  show (Bracketted a) = "<" ++ show a ++ ">"

instance Show Mail_identity where
  show (Single_address m) = show m
  show (Target {get'target_name = n,
                get'target_address = a})
    = show n ++ " " ++ show a

instance Show Header_content where
  show (Header_single_address address) = show address
  show (Address_list addresses) =
    concatWith ",\n\t" . map show $ addresses
  show (Header_string s) = s

instance Show Header where
  show (Header {get'header_name = n,
                get'header_content = c}) = n ++ ": " ++ show c

------------- Read

-- read a type that's really a specialization of String
easyreadstr c k r = [ (c s, s') | (s,s') <- readsPrec (k+1) r]

instance Read Quoted_string where
  readsPrec = easyreadstr QS
  
instance Read Box_name where
  readsPrec = easyreadstr Box_name

instance Read  Domain_name where
  readsPrec = easyreadstr Domain_name
  
-- data Mail_address = At Box_name Domain_name
-- data Bracketted_address = Bracketted Mail_address
-- data Mail_identity = Single_address Mail_address
--                    | Target
--                      {get'target_name :: Quoted_string,
--                       get'target_address :: Bracketted_address}
-- type Header_name = String
-- data Header_content = Header_single_address Mail_identity
--                     | Address_list [Mail_identity]
--                     | Header_string String
-- data Header = Header {get'header_name :: Header_name,
--                       get'header_content :: Header_content}
------------- Tests

main = do
  let 
    qs = QS "a quoted string"
    ma = Box_name "mikaeldusenne" `At` Domain_name "gmail.com"
    mb = Box_name "mikaeldusenne" `At` Domain_name "mac.com"
    mc = Box_name "camivandevelde" `At` Domain_name "gmail.com"
    ba = Bracketted ma
    ia = Target {get'target_name = QS "Mikaël Dusenne", get'target_address = ba}
    la = Address_list [Single_address mb,
                       Single_address mc,
                       ia]
    toh = to la
    cch = cc la
    fromh = from ia
  putStrLn $ "hello to " ++ show qs ++ " !!!"
  print ma
  print ba
  print ia
  print la
  print "--------"
  print cch
  print "--------"
  print fromh
