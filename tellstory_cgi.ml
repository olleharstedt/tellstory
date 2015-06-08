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

(* TODO: Included by default? *)
type ('a, 'b) result =
  | Ok of 'a
  | Error of 'b

(* Create an HTML escaping function for the UTF-8 encoding. *)
let escape_html = Netencoding.Html.encode ~in_enc:`Enc_utf8 ()

(**
 * Read examples as <option> from dir ../examples
 * Use with <select>
 *
 * @return string
 *)
let get_examples () =
  let open Unix in
  let result = ref ([] : (string * string) list) in
  let d = opendir "../examples" in
  (try while true do begin
    let file = readdir d in
    (* Skip these two "files" *)
    if file = "." || file = ".." then
      (* Do nothing *)
      ()
    else begin
      (* Read file *)
      let in_channel = open_in (sprintf "../examples/%s" file) in
      let file_content = ref "" in
      (try while true do begin
        let line = input_line in_channel in
        file_content := !file_content ^ (escape_html line) ^ "\n"
      end done
      with End_of_file -> close_in in_channel);
      result := (file, !file_content) :: !result
    end
  end done
  with End_of_file -> closedir d);

  List.fold_left (fun sum (name, content) -> 
    sprintf "%s<option value='%s'>%s</option>" sum content name
  ) "" !result

(**
 * Main cgi function
 *
 * @param cgi Netcgi.cgi
 * @return unit
 *)
let process (cgi : Netcgi.cgi) =
	cgi#set_header
		~cache:`No_cache
		~content_type:"text/html; charset=\"utf-8\"\r\n\r\n"
		();

  let story = cgi#argument_value "story" in
  let example = cgi#argument_value "example" in

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
    let xml_or_error = try Ok (Xml.parse_string story) with
      | Xml.Error (msg, pos) ->
          Error (sprintf "Error while parsing XML on line %d: %s" (Xml.line pos) (Xml.error_msg msg))
    in
    begin match xml_or_error with
    | Ok xml ->
        begin try Tellstory.story_to_string xml state with
        | ex ->
            (escape_html (Printexc.to_string ex))
        end
    | Error error_msg ->
        error_msg
    end
  end else "" in

  let examples = get_examples () in

  let html = sprintf
    "<!DOCTYPE html>
      <html>
        <head>
          <title>Tellstory</title>
          <script>
            function example_changed(that) {
              var textarea = document.getElementById('story_textarea');
              var file_content = that.options[that.selectedIndex].value;
              //var str = file_content.replace(/(?:\\r\\n|\\r|\\n)/g, '<br />');
              textarea.value = file_content;
              //console.log(str);
            }
          </script>
        </head>
        <body>
          <p>Randomize text using XML</p>
          <p>Examples:</p>
          <!--
          <form method='post' action='tellstory.cgi'>
            <select id='examples' name='example' onchange='this.form.submit();'></select><br /><br />
          </form>
          -->
          <select id='examples' name='example' onchange='example_changed(this);'>%s</select><br /><br />
          <form method='post' action='tellstory.cgi'>
            <textarea id='story_textarea' name='story' cols='100' rows='20'>%s</textarea><br /><br />
            <input type='submit' value='Tell story' />
          </form>
          <p>%s</p>
        </body>
      </html>"
    examples
    (if example = "" then story else example)
    result
  in

  cgi#out_channel#output_string html;

	(* Flush the output buffer. *)
	cgi#out_channel#commit_work();

  cgi#finalize()

(**
 * Run something
 *)
let _ =
  let config = Netcgi.default_config in
  let buffered _ ch = new Netchannels.buffered_trans_channel ch in
  Netcgi_cgi.run ~config:config ~output_type:(`Transactional buffered) process
