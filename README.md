# Mail Redactor

[asciinema](https://asciinema.org/a/181832)

NB: This project is currently very dependent on my personal setup.

This program takes as input the body of a mail written in (almost, cf below) markdown, and outputs a multipart-email containing the original text, the html pandoc-compiled text, and, when necessary, attachments to the mail.

*It does not handle any mail header, the reason being that I use it with Emacs, which was already taking care of those headers for me.*

It allows me to write my mails in markdown and having them directly converted to html.

If I enter the path to an existing file on a line, it adds it as an attachment to the mail, and if it's an image, it inlines it in the mail body, and if it's a text file, it also adds an inline preview in the body of the mail with syntax highlighting based on the extension of the file's name.

I did not perform extensive testing of the display of mails, but it should be mostly working. (the trickiest part is scrollable divs inside emails to display text files preview, this does not seem to be extremely well supported, but it works well with gmail!)

**Here is an example of a mail as I write it in Emacs:**

![markdown](/data/screenshots/mail_emacs.png/?raw=true "Mail redaction from Emacs")

**And the resulting mail:**

![gmail](/data/screenshots/mail_output.gif/?raw=true "Result")

Notice the smileys!

The mail's plain text content is encoded following the [Quoted-Printable](https://en.wikipedia.org/wiki/Quoted-printable) specifications, the html part is in base64.

