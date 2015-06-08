(**
 * CGI-file for tellstory
 *
 * command to get a file:
 * curl https://raw.githubusercontent.com/olleharstedt/tellstory/master/examples/deck.xml
 *
 * get list of files in /examples
 * curl https://api.github.com/repos/olleharstedt/tellstory/contents/examples
 *
 * @since 2015-06-07
 * @author Olle Härstedt
 *)

open Printf
open ListLabels
open Tellstory

(* Create an HTML escaping function for the UTF-8 encoding. *)
let escape_html = Netencoding.Html.encode ~in_enc:`Enc_utf8 ()

let process (cgi : Netcgi.cgi) =
	cgi#set_header
		~cache:`No_cache
		~content_type:"text/html; charset=\"utf-8\"\r\n\r\n"
		();

  let story = cgi#argument_value "story" in

  let result = if story <> "" then begin
    Random.self_init ();
    (** Create module with dice function module *)
    let module Tellstory = Tellstory.Make(
      struct
        let dice n =
          Random.int n
      end
    ) in
    let state = Tellstory.init_state () in
    let xml = try Some (Xml.parse_string story) with 
      | Xml.Error (msg, pos) ->
          cgi#out_channel#output_string (sprintf "Error while parsing XML file %d: %s" (Xml.line pos) (Xml.error_msg msg));
          None
    in
    begin match xml with
    | Some xml ->
        begin try Tellstory.story_to_string xml state with
        | ex ->
            Printexc.to_string ex
        end
    | None ->
        "No XML?"
    end
  end else "" in


  let html = sprintf
    "<!DOCTYPE html>
      <html>
        <head>
          <title>Tellstory</title>
        </head>
        <body>
          <p>Randomize text using XML</p>
          <form method='post' action='tellstory.cgi'>
            <textarea name='story' cols='50' rows='8'>%s</textarea>
            <input type='submit' value='Tell story' />
          </form>
          <p>%s</p>
        </body>
      </html>"
    story
    result
  in

  cgi#out_channel#output_string html;

	(* Flush the output buffer. *)
	cgi#out_channel#commit_work();

  cgi#finalize()

let _ =
  let config = Netcgi.default_config in
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  Netcgi_cgi.run ~config:config ~output_type:(`Transactional buffered) process
