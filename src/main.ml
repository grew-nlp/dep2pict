open Printf
open Log
open Dep2pict

open Global

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
  "  -s | --sentid    <string> identifier of the sentence to display (incompatible with -p | --position)";
  "  -p | --position  <int> number of the dep structure to display when input file contains sequence (incompatible with -s | --sentid)";
  "  -d | --debug     add on_mouse_over tips in svg output and set verbose mode for font utilities";
  "  -b | --batch     Unformated error message that can be used in a pipeline or a web app";
  "  --special_chars  <file> give a set of chars (one char by line) that are considered as 1.5 width of 'X' (for korean chars for instance)";
  "================================================================================";
]


let requested_sentid = ref None

(* the name of the file containing special chars (like korean chars) *)
let special_chars = ref None


let rec parse_arg = function
  | [] -> ()
  | "-v"::_ | "--version"::_ -> printf "%s\n%!" version; exit 0
  | "-h"::_ | "--help"::_ -> printf "%s\n%!" usage; exit 0

  | "-p"::i::tail
  | "--position"::i::tail -> current_position := ((int_of_string i)-1); parse_arg tail

  | "-s"::s::tail
  | "--sentid"::s::tail -> requested_sentid := Some s; parse_arg tail

  | "--special_chars"::s::tail -> special_chars := Some s; parse_arg tail

  | "-d"::tail | "--debug"::tail -> debug := true; parse_arg tail

  | "-b"::tail | "--batch"::tail -> batch := true; parse_arg tail

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

(* -------------------------------------------------------------------------------- *)
let _ =
  let () = parse_arg (List.tl (Array.to_list Sys.argv)) in

  if !debug then Dep2pict.set_verbose ();

  begin
    match !special_chars with
    | None -> ()
    | Some filename -> Dep2pict.load_special_chars filename
  end;

  (* check for input_file and load file if any *)
  begin
    match !input_file with
    | None -> ()
    | Some in_file when not (Sys.file_exists in_file) -> Log.fcritical "The input file %s doesn't exist." in_file
    | Some in_file -> load in_file
  end;

  (* check for focus *)
  begin
    match (!current_data, !current_position, !requested_sentid) with
    | (Conll _, _, Some sentid) -> (
      try search_sentid sentid
      with Not_found ->
        Log.fwarning "sentid %s cannot be found, set position to 0" sentid;
        current_position := 0; update_source ()
      )
    | (Conll arr, p, None) when p < 0 || p >= (Array.length arr) ->
      Log.fwarning "position %d is out of bounds, set position to 0" p;
      current_position := 0; update_source ()
    | (Conll _, p, None) -> current_position := p; update_source ()
    | (_, _, Some _) -> Log.fcritical "Options --sentid can be used only with CONLL input"
    | (_, _, None) -> ()
  end;

  match !output_file with
    | None -> Gui.main ()
    | Some out_file ->
      try
        let graph = match !current_data with
        | No_data -> critical "No input data loaded"
        | Dep (g,_) -> g
        | Conll _ -> Dep2pict.from_conll ~conll:(!current_source) in
        begin
          match Format.get out_file with
          | Format.Svg -> Dep2pict.save_svg ~filename:out_file graph
          | Format.Pdf -> Dep2pict.save_pdf ~filename:out_file graph
          | Format.Png -> Dep2pict.save_png ~filename:out_file graph
          | Format.Dep -> (
            match (!current_data, !current_position) with
            | (Conll arr, p) -> File.write out_file (Dep2pict.conll_to_dep ~conll:(snd arr.(p)))
            | _ -> critical "<dep> output format is available only for <conll> inputs"
          )
          | f -> critical "<%s> is not a valid output format" (Format.to_string f)
        end;
        Log.finfo "File %s generated." out_file
      with
      | Dep2pict.Parse_error msgs -> List.iter (fun (l,m) -> printf "Line %d: %s\n" l m) msgs; critical "Parse error !!"
      | Dep2pict.Id_already_in_use_ id -> critical "Id already used: %s" id
      | Dep2pict.Conll_format msg -> critical "Invalid CONLL line: %s" msg
      | Dep2pict.Unknown_index id -> critical "Can't find index: %s" id
      | Dep2pict.Loop_in_dep msg -> critical "Loop in dependency: %s" msg
      | exc -> critical "Unexpected exception <%s>, please report" (Printexc.to_string exc)
