(**
 * CGI-file for tellstory
 *
 * @since 2015-06-07
 * @author Olle Härstedt
 *)

open Printf

(* Create an HTML escaping function for the UTF-8 encoding. *)
let escape_html = Netencoding.Html.encode ~in_enc:`Enc_utf8 ()

let process (cgi : Netcgi.cgi) =
	cgi#set_header
		~cache:`No_cache
		~content_type:"text/html; charset=\"utf-8\"\r\n\r\n"
		();

  cgi#out_channel#output_string "<!DOCTYPE html>
    <html>
      <head>
      </head>
      <body>
        <p>Randomize text using XML.</p>
        <form method='post' action='tellstory.cgi'>
          <textarea name='story' cols='50' rows='8'>
          </textarea>
        </form>
      </body>
    </html>";

	(* Flush the output buffer. *)
	cgi#out_channel#commit_work();

  cgi#finalize()


let _ =
  let config = Netcgi.default_config in
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  (*Netcgi_cgi.run ~config:config ~output_type:(`Transactional buffered) process*)
  Netcgi_test.run ~config:config ~output_type:(`Transactional buffered) process
