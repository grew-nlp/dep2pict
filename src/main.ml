open Printf
open Log
open Dep2pict

open Global

IFDEF BUILD_GUI THEN
open Gui
let gui () = Gui.main ()
ELSE
let gui () = Log.critical "Gui not available"
END

let version = VERSION

let _ =
  Log.set_active_levels [`INFO];
  Log.set_info_label "DEP2PICT";
  Log.set_critical_label "DEP2PICT";
  Log.set_write_to_log_file false;
  Log.set_info_foreground Log.f_green;
  Log.set_critical_foreground Log.f_red;
  Log.set_critical_background Log.b_default;
  Log.set_show_time true

let usage = String.concat "\n" [
  "================================================================================";
  "=-=-=-=-= dep2pict: a tool to draw dependency graphs =-=-=-=-=";
  "";
  "Usage:";
  "  * dep2pict <options> input_file output_file    convert input_file into output_file" ;
  "  * dep2pict <options> input_file                run the GUI with the given file";
  "  * dep2pict <options>                           run the GUI with an empty graph";
  "  * dep2pict (-h | --help)                       display this help";
  "  * dep2pict (-v | --version)                    display version number ("^version^")";

  "";
  "Formats are guessed from file extension:";
  "  * accepted input formats are: dep, conll, xml";
  "  * accepted output formats are: png, svg, pdf, dep";
  "";
  "Options:";
  "  -i | --infos     <string> select infos to display for conll input:";
  "                   <string> is a '|' separated list of atoms from: \"lemma\", \"pos\", \"lpos\", \"all\",";
  "                   (default=\"pos\")";
  "  -s | --sentid    <string> identifier of the sentence to display (incompatible with -p | --position)";
  "  -p | --position  <int> number of the dep structure to display when input file contains sequence (incompatible with -s | --sentid)";
  "  -d | --debug     add on_mouse_over tips in svg output (for debug purpose)";
  "================================================================================";
]

let logo = String.concat "\n" [
"[GRAPH] { word_spacing=0; opacity=50; scale=300 }";
"[WORDS] {";
"  A { word=\"Dep\"; forecolor=purple; }";
"  B { word=\"2\"; forecolor=orange; }";
"  C { word=\"pict\"; forecolor=pink; }";
"}";
"[EDGES] {";
"  A -> B { color=red; }";
"  C -> B { color=blue;}";
"  B -> A { bottom; color=yellow; }";
"  B -> C { bottom; color=green; }";
"}";
]

let requested_sentid = ref None

let rec parse_arg = function
  | [] -> ()
  | "-v"::_ | "--version"::_ -> printf "%s\n%!" version; exit 0
  | "-h"::_ | "--help"::_ -> printf "%s\n%!" usage; exit 0

  | "-i"::i::tail
  | "--infos"::i::tail -> current_infos := Str.split (Str.regexp " *| *") i; parse_arg tail

  | "-p"::i::tail
  | "--position"::i::tail -> current_position := Some (int_of_string i); parse_arg tail

  | "-s"::s::tail
  | "--sentid"::s::tail -> requested_sentid := Some s

  | "-d"::tail | "--debug"::tail -> debug := true; parse_arg tail

  | s::_ when s.[0] = '-' -> Log.fcritical "Unknwon option \"%s\"" s

  | anon :: tail ->
    begin
      match !input_file with
        | None -> input_file := Some anon
        | Some _ ->
          match !output_file with
            | None -> output_file := Some anon
            | Some _ -> Log.fcritical "At most two anonymous arguments are allowed, don't know what to do with \"%s\"" anon
    end;
    parse_arg tail



let _ =
  let () = parse_arg (List.tl (Array.to_list Sys.argv)) in

  (* check for input_file *)
  match !input_file with
    | None -> current_source := logo; gui ()
    | Some in_file when not (Sys.file_exists in_file) -> Log.fcritical "The input file %s doesn't exist." in_file
    | Some in_file -> load in_file;

      (* check for focus *)
      begin
        match (!current_position, !requested_sentid) with
          | (None, None) when Array.length !current_array = 0 -> ()
          | (None, None) -> current_position := Some 0
          | (Some p, None) when p < 0 || p >= (Array.length !current_array) ->
            Log.fwarning "position %d is out of bounds, set position to 0" p;
            current_position := Some 0
          | (Some p, None) -> current_position := Some p
          | (None, Some sentid) -> (
            try search_sentid sentid
            with Not_found -> Log.fwarning "sentid %s cannot be found, set position to 0" sentid;
          )
          | (Some _, Some _) -> Log.fcritical "Options --position and --sentid are incompatible"
      end;

      update_source ();

      match !output_file with
        | None -> gui ()
        | Some out_file ->
          begin
            try
              match (get_format in_file, get_format out_file) with
                | (Xml, Png) -> ignore (Dep2pict.fromXmlFileToPng in_file out_file (get_pos ()))
                | (Dep, Png) -> ignore (Dep2pict.fromDepFileToPng in_file out_file)
                | (Conll, Png) -> ignore (Dep2pict.fromConllStringToPng ~infos:!current_infos !current_source out_file)

                | (Xml, Svg) -> ignore (Dep2pict.fromXmlFileToSvgFile ~debug:(!debug) in_file out_file (get_pos ()))
                | (Dep, Svg) -> ignore (Dep2pict.fromDepFileToSvgFile ~debug:(!debug) in_file out_file)
                | (Conll, Svg) -> ignore (Dep2pict.fromConllStringToSvgFile ~infos:!current_infos !current_source out_file)

                | (Xml, Pdf) -> ignore (Dep2pict.fromXmlFileToPdf in_file out_file (get_pos ()))
                | (Dep, Pdf) -> ignore (Dep2pict.fromDepFileToPdf in_file out_file)
                | (Conll, Pdf) -> ignore (Dep2pict.fromConllStringToPdf ~infos:!current_infos !current_source out_file)

                | (Conll, Dep) -> Dep2pict.fromConllStringToDep ~infos:!current_infos !current_source out_file

                | (Dep, Dep)
                | (Xml, Dep) -> Log.fcritical "Conversion from Xml or Dep into Dep is not implemented. Please contact developers if your really need it!"
                | (_, Xml) -> Log.fcritical "Conversion to Xml is not implemented. Please contact developers if your really need it!"
                | (Pdf, _) -> Log.fcritical "pdf in not a valid input format"
                | (Svg, _) -> Log.fcritical "svg in not a valid input format"
                | (Png, _) -> Log.fcritical "png in not a valid input format"
                | (_, Conll) -> Log.fcritical "conll in not a valid output format"
            with
	      | Dep2pict.Parse_error msgs -> List.iter (fun (l,m) -> printf "Line %d: %s\n" l m) msgs; Log.fcritical "Parse error !!"
	      | Dep2pict.Id_already_in_use_ id -> Log.fcritical "Id already in use : %s" id
	      | Dep2pict.Unknown_index id -> Log.fcritical "Can't find index: %s" id
	      | Dep2pict.Loop_in_dep msg -> Log.fcritical "Loop in dependency : %s" msg
          end;
          Log.finfo "File %s generated." out_file

