module Mail where

import Mail_headers


type Multiparts = [Mail]

data Mail_content = Empty_mail
                  | Multipart_mail Multiparts
                  | Simple_mail String


data Mail = Mail {get'headers :: Headers,
                  get'content :: Mail_content}


------------- Show





------------- Read

