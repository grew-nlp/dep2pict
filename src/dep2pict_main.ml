open Printf
open Dep2pictlib
open Conll
open Grewlib

open Global

let version =
  match Build_info.V1.version () with
  | Some v -> Printf.sprintf "%s" (Build_info.V1.Version.to_string v)
  | None -> Printf.sprintf "dev"


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

  (* does nothing (used by dep2pict-gui to check for existence of dep2pict program) *)
  | "--check"::_ -> exit 0

  | "-d"::tail | "--debug"::tail -> debug := true; parse_arg tail

  | "-b"::tail | "--batch"::tail -> batch := true; parse_arg tail

  | "--no_root"::tail -> no_root := true; parse_arg tail

  | "-rtl":: tail | "--right_to_left":: tail -> rtl := true; parse_arg tail
  | s::_ when s.[0] = '-' -> Log.fail "Unknwon option \"%s\"" s

  | anon :: tail ->
    begin
      if !first
      then (input_file := Some anon; first := false)
      else
        match !output_file with
        | None -> output_file := Some anon
        | Some _ -> Log.fail "At most two anonymous arguments are allowed, don't know what to do with \"%s\"" anon
    end;
    parse_arg tail

let json_apply json_in json_out =
  match json_in with
  | `List l ->
    let (new_json : Yojson.Basic.t) = `List (List.map (
      function
      | `Assoc item ->
      begin
         match List.assoc_opt "dep_file" item with
         | Some (`String dep_file) ->
          let out_file = (Filename.chop_extension dep_file) ^ ".svg" in
          let dep = Dep2pictlib.from_dep (File.read dep_file) in
          Dep2pictlib.save_svg ~filename:out_file dep;
          let new_fields =
            match Dep2pictlib.highlight_shift () with
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

let filter = function
  | "wordform" | "textform" -> false
  | _ -> true

(* -------------------------------------------------------------------------------- *)
let main () =
  let arg_list = List.tl (Array.to_list Sys.argv) in
  let () = parse_arg arg_list in

  if !debug then Dep2pictlib.set_verbose ();

    match (!input_file, !output_file) with
    | (None,_) | (_,None) -> Log.warning "Two main arguments needed"; printf "%s\n%!" usage; exit 0
    | (Some in_file, Some out_file) ->
      if (Format.get in_file) = Format.Json
      then json_apply (Yojson.Basic.from_file in_file) out_file
      else
      try
        load in_file;
          set_position ();
          let graph = match (!current_data, !current_position) with
          | (Dep g,_) -> g
          | (Conll [||],_) -> error ~file: in_file "Empty Conll file"
          | (Conll arr, pos) -> snd arr.(pos) |> Conll.to_json |> Graph.of_json |> Graph.to_dep ~filter ~no_root:(!no_root) ~config:(Conll_config.build "ud") |> (fun d -> Dep2pictlib.from_dep d) in
          begin
            match Format.get out_file with
            | Format.Svg -> Dep2pictlib.save_svg ~filename:out_file graph
            | Format.Pdf -> Dep2pictlib.save_pdf ~filename:out_file graph
            | Format.Png -> Dep2pictlib.save_png ~filename:out_file graph
            | Format.Dep -> (
              match (!current_data, !current_position) with
              | (Conll arr, p) ->
              let dep = snd arr.(p) |> Conll.to_json |> Graph.of_json |> Graph.to_dep ~filter ~config:(Conll_config.build "ud") in
                File.write out_file dep
              | _ -> critical "<dep> output format is available only for <conll> inputs"
            )
            | f -> critical "<%s> is not a valid output format" (Format.to_string f)
          end;
          ANSITerminal.eprintf [ANSITerminal.green] "File %s generated.\n" out_file
      with
      | Error json -> raise (Error json)
      | Dep2pictlib.Error json -> raise (Error json)
      | Conll_error json -> raise (Error json)
      | Sys_error data -> error ~file: in_file ~data "Sys_error"
      | exc -> error ~file: in_file ~data:(Printexc.to_string exc) "Unexpected exception, please report"

let _ =
  try main ()
  with Error json -> critical "%s" (Yojson.Basic.pretty_to_string json)