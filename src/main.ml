open Printf
open Log
open Conll
open Dep2pict

open Global

let version = VERSION

let _ =
  Log.set_active_levels [`INFO; `WARNING];
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
  "  * dep2pict <options> input_file output_file    convert input_file into output_file";
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


let first = ref true
let rec parse_arg = function
  | [] -> ()
  | "-v"::_ | "--version"::_ -> printf "%s\n%!" version; exit 0
  | "-h"::_ | "--help"::_ -> printf "%s\n%!" usage; exit 0

  | "-p"::i::tail
  | "--position"::i::tail -> current_position := ((int_of_string i)-1); parse_arg tail

  | "-s"::s::tail
  | "--sentid"::s::tail -> requested_sentid := Some s; parse_arg tail

  (* does nothing (allow to pyqt to check for existence of the program) *)
  | "--check"::_ -> exit 0

  | "--special_chars"::s::tail -> special_chars := Some s; parse_arg tail

  | "-d"::tail | "--debug"::tail -> debug := true; parse_arg tail

  | "-b"::tail | "--batch"::tail -> Log.set_active_levels []; batch := true; parse_arg tail

  | "-rtl":: tail | "--right_to_left":: tail -> rtl := true; parse_arg tail
  | s::_ when s.[0] = '-' -> Log.fcritical "Unknwon option \"%s\"" s

  | anon :: tail ->
    begin
      if !first
      then (input_file := anon; first := false)
      else
        match !output_file with
        | None -> output_file := Some anon
        | Some _ -> Log.fcritical "At most two anonymous arguments are allowed, don't know what to do with \"%s\"" anon
    end;
    parse_arg tail

let json_apply json_in json_out =
  match json_in with
  | `List l ->
    let (new_json : Yojson.Basic.json) = `List (List.map (
      function
      | `Assoc item ->
      begin
         match List.assoc_opt "dep_file" item with
         | Some (`String dep_file) ->
          let out_file = (Filename.chop_extension dep_file) ^ ".svg" in
          let dep = Dep2pict.from_dep (File.read dep_file) in
          Dep2pict.save_svg ~filename:out_file dep;
          let new_fields =
            match Dep2pict.highlight_shift () with
            | Some f -> [("svg_file", `String out_file); ("shift", `Float f)]
            | None -> [("svg_file", `String out_file)] in
          `Assoc (item @ new_fields)
         | _ -> Log.warning "Json items should contain a \"del_file\" field"; exit 0
      end
      | _ -> Log.warning "Json input file should be a list of Assoc list"; exit 0
      ) l) in
      let out_ch = open_out json_out in
      Printf.fprintf out_ch "%s\n" (Yojson.Basic.pretty_to_string (new_json));
      close_out out_ch
  | _ -> Log.warning "Json input file should be a list"; exit 0
  (* (Yojson.Basic.pretty_to_string json) *)

(* -------------------------------------------------------------------------------- *)
let main () =
  let arg_list = List.tl (Array.to_list Sys.argv) in
  let () = parse_arg arg_list in

  if !debug then Dep2pict.set_verbose ();

  begin
    match !special_chars with
    | None -> ()
    | Some filename -> Dep2pict.load_special_chars filename
  end;

  (* check for input_file and load file if any *)
    match !output_file with
    | None ->
      begin
        match Unix.system ("dep2pict_qt" ^ " " ^ (String.concat " " arg_list)) with
          | Unix.WEXITED 127 -> Log.warning "It seems that dep2pict_qt is not installed on your system. See [http://dep2pict.loria.fr/installation] for more information"
          | _ -> ()
        end
    | Some out_file ->
      if (Format.get !input_file) = Format.Json
      then json_apply (Yojson.Basic.from_file !input_file) out_file
      else
      try
        load !input_file;
          set_position ();
          let graph = match (!current_data, !current_position) with
          | (Dep g,_) -> g
          | (Conll [||],_) -> error ~file: !input_file "Empty Conll file"
          | (Conll arr, pos) ->
          Dep2pict.from_conll ~rtl:!rtl ~conll:(snd arr.(pos)) in
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
      | Error json -> raise (Error json)
      | Dep2pict.Error json -> raise (Error json)
      | Conll_types.Error json -> raise (Error json)
      | Sys_error data -> error ~file: !input_file ~data "Sys_error"
      | exc -> error ~file: !input_file ~data:(Printexc.to_string exc) "Unexpected exception, please report"

let _ =
  try main ()
  with Error json -> critical "%s" (Yojson.Basic.pretty_to_string json)