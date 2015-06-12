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
 * Read the content of file and return string with escaped chars for HTML
 *
 * @param filename string
 * @return string
 *)
let read_file_escape filename =
  let in_channel = open_in filename in
  let file_content = ref "" in
  (try while true do begin
    let line = input_line in_channel in
    file_content := !file_content ^ (escape_html line) ^ "\n"
  end done
  with End_of_file -> close_in in_channel);
  (*eprintf "file_content = %s" !file_content;*)
  !file_content

(**
 * Read file, return string, no escape
 *
 * @param filename string
 * @return string
 *)
let read_file filename =
  let in_channel = open_in filename in
  let file_content = ref "" in
  (try while true do begin
    let line = input_line in_channel in
    file_content := !file_content ^ line
  end done
  with End_of_file -> close_in in_channel);
  (*eprintf "file_content = %s" !file_content;*)
  !file_content

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
      let filename = (sprintf "../examples/%s" file) in
      let file_content = read_file_escape filename in
      result := (file, file_content) :: !result
      (*
      let in_channel = open_in (sprintf "../examples/%s" file) in
      let file_content = ref "" in
      (try while true do begin
        let line = input_line in_channel in
        file_content := !file_content ^ (escape_html line) ^ "\n"
      end done
      with End_of_file -> close_in in_channel);
      (*eprintf "file_content = %s" !file_content;*)
      result := (file, !file_content) :: !result
      *)
    end
  end done
  with End_of_file -> closedir d);

  List.fold_left (fun sum (name, content) -> 
    sprintf "%s<option value=\"%s\">%s</option>" sum content name
  ) "" !result

(**
 * Tells a story!
 *
 * @param story string Xml string
 *)
let tellstory story =
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
  let op = cgi#argument_value "op" in

  let html = match op with
  | "" ->
    let result = if story <> "" then begin
      tellstory story
    end else "" in

    let examples = get_examples () in

    sprintf
      "<!DOCTYPE html>
        <html>
          <head>
            <title>Tellstory</title>
            <script>
              function example_changed(that) {
                var textarea = document.getElementById('story_textarea');
                var file_content = that.options[that.selectedIndex].value;
                textarea.value = file_content;
              }
            </script>
          </head>
          <body>
            <h2>Randomize text using XML</h2>
            <p>Read the manual <a href='https://github.com/olleharstedt/tellstory'>here</a>.</p>
            <p>Examples:</p>
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
  | "showsinglefile" ->
      let filename = cgi#argument_value "filename" in
      begin match filename with
      | "" ->
          "No filename given"
      | filename ->
          let file_content = read_file (sprintf "../examples/%s.xml" filename) in
          tellstory file_content
      end
  | unknown_op ->
      sprintf "Unknown op: %s" unknown_op
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
