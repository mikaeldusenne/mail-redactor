#!/bin/sh


## update mail database
dt_file="$HOME/.perso/mail_index_date"
dt="$(cat $dt_file)"
find $HOME/Mail/ -type 'f' -newermt $dt | pv -l | generate_mail_index.sh \
	&& (echo -n $(date --date yesterday +"%Y-%m-%d") > $dt_file)

# ## ged mail id from command-line-argument
# id="$(echo "$1" | grep -iF 'message-id:' | sed 's/^message-id:\s*\(.*\)\s*$/\1/I')"
# echo "$id"
# ##


########
ghc ./Parse_mail.hs -i/home/mika/programmation/haskell/MyRealLib/ && \
	ghc ./Mail_redactor.hs -i/home/mika/programmation/haskell/MyRealLib/ && \
	./Mail_redactor "$1"

exit 0
#########################################################################################


echo -n > redacted_mail.txt
echo -n > body


ghc ./Parse_mail.hs -i/home/mika/programmation/haskell/MyRealLib/ && \
	grep -F "$id" ~/.perso/mail_index | grep INBOX | awk -F§ '{print $1}' | xargs cat \
		| tee mail_original.txt | ./parse_mail \
									  > headers

[ $? = 0 ] && vim headers && vim body \
	&& (cat headers >> redacted_mail.txt) \
	&& (cat body | ./convert_markdown >> redacted_mail.txt) 

# > redacted_mail.txt \
# 									  && vim redacted_mail.txt 

echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"
cat redacted_mail.txt
echo "@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@"

ask "send the mail?" && (cat redacted_mail.txt | sendmail --read-envelope-from -t && echo "mail sent" || echo "failure") || echo "cancelled."

