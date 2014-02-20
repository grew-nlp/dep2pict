open Printf
open Log
open Dep2pict

IFDEF BUILD_GUI THEN
open Gui
let gui ?infos ?file () = Gui.main ?infos ?file ()
ELSE
let gui ?infos ?file () = Log.critical "Gui not available"
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
  "=-=-=-=-= dep2pict a tool to draw dependency graphs =-=-=-=-=";
  "";
  "Usage:";
  "  * dep2pict <options> input_file output_file    convert input_file into output_file" ;
  "  * dep2pict <options> input_file                run the GUI with the given file";
  "  * dep2pict <options>                           run the GUI with an empty graph";
  "";
  "Formats are guessed from file extension:";
  "  * input formats are: dep, conll, xml";
  "  * output formats are: png, svg, pdf, dep";
  "";
  "Options:";
  "  -i | --infos     <string> select infos to display: a '|' separated list of atoms from: \"lemma\", \"pos\", \"lpos\", \"all\", a feature name (default=\"pos\")";
  "  -p | --position  <int> number of the dep structure to display when input file contains sequence (default=0)";
  "  -v | --version   display version number ("^version^")";
  "  -h | --help      show this help";
  "  -t | --tips      add on_mouse_over tips in svg output (for debug purpose)";
  "================================================================================";
]

let input_file = ref None
let output_file = ref None

let position = ref 0
let tips = ref false

let infos = ref []

let rec parse_arg = function 
  | [] -> ()
  | "-v"::_ | "--version"::_ -> printf "%s\n%!" version; exit 0
  | "-h"::_ | "--help"::_ -> printf "%s\n%!" usage; exit 0
  | "-i"::i::tail 
  | "--infos"::i::tail -> infos := Str.split (Str.regexp " *| *") i

  | "-p"::i::tail 
  | "--position"::i::tail -> position := (int_of_string i); parse_arg tail

  | "-t"::tail | "--tips"::tail -> tips := true; parse_arg tail

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

type format =  Dep | Conll | Xml | Png | Svg | Pdf
exception Found of format
let get_format file =
  try 
    List.iter
      (fun (ext, fmt) ->
        if Filename.check_suffix file ext
        then raise (Found fmt)
      ) [ ("dep", Dep); ("conll",Conll); ("xml",Xml); ("png",Png); ("svg",Svg); ("pdf",Pdf) ];
    Log.fcritical "Unkwnow file extension for file \"%s\"" file
  with Found fmt -> fmt


let _ = 
  let () = parse_arg (List.tl (Array.to_list Sys.argv)) in 

  match (!input_file, !output_file) with
    | (None, None) -> gui ~infos:(!infos) ()
    | (Some file,_) when not (Sys.file_exists file) -> Log.fcritical "The input file %s doesn't exist." file
    | (Some file, None) -> gui ~infos:(!infos) ~file ()
    | (Some in_file, Some out_file) ->
      begin
        try
          match (get_format in_file, get_format out_file) with
            | (Xml, Png) -> ignore (Dep2pict.fromXmlFileToPng in_file out_file !position)
            | (Dep, Png) -> ignore (Dep2pict.fromDepFileToPng in_file out_file)
            | (Conll, Png) -> ignore (Dep2pict.fromConllFileToPng ~infos:!infos in_file out_file)

            | (Xml, Svg) -> ignore (Dep2pict.fromXmlFileToSvgFile ~debug:(!tips) in_file out_file !position)
            | (Dep, Svg) -> ignore (Dep2pict.fromDepFileToSvgFile ~debug:(!tips) in_file out_file)
            | (Conll, Svg) -> ignore (Dep2pict.fromConllFileToSvgFile ~debug:(!tips) ~infos:!infos in_file out_file)

            | (Xml, Pdf) -> ignore (Dep2pict.fromXmlFileToPdf in_file out_file !position)
            | (Dep, Pdf) -> ignore (Dep2pict.fromDepFileToPdf in_file out_file)
            | (Conll, Pdf) -> ignore (Dep2pict.fromConllFileToPdf ~infos:!infos in_file out_file)
          
            | (Conll, Dep) -> Dep2pict.fromConllFileToDep ~infos:!infos in_file out_file

            | (Dep, Dep) 
            | (Xml, Dep) -> Log.fcritical "Conversion from Xml or Dep into Dep is not implemented. Please contact developers if your really need it!"
            | (_, Xml) -> Log.fcritical "Conversion to Xml is not implemented. Please contact developers if your really need it!"
            | (Pdf, _) -> Log.fcritical "pdf in not a valid input format"
            | (Svg, _) -> Log.fcritical "svg in not a valid input format"
            | (Png, _) -> Log.fcritical "png in not a valid input format"
            | (_, Conll) -> Log.fcritical "conll in not a valid output format"
        with
	  | Dep2pict.Parse_error msgs -> List.iter (fun (l,m) -> printf "Line %d: %s\n" l m) msgs; Log.fcritical "Parse error"
	  | Dep2pict.Id_already_in_use_ id -> Log.fcritical "Id already in use : %s" id
	  | Dep2pict.Unknown_index id -> Log.fcritical "Can't find index: %s" id
	  | Dep2pict.Loop_in_dep msg -> Log.fcritical "Loop in dependency : %s" msg
      end;
      Log.finfo "File %s generated." out_file
    | (None, Some _) -> Log.bug "Some output without input!"

