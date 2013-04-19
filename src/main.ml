open Printf
open Log
open Dep2pict
IFDEF BUILD_GUI THEN
open Gui
END

let _ = 
	Log.set_active_levels [`INFO];
	Log.set_info_label "DEP2PICT";
	Log.set_critical_label "DEP2PICT";
	Log.set_write_to_log_file false;
	Log.set_info_foreground Log.f_green; 
	Log.set_critical_foreground Log.f_red;
	Log.set_critical_background Log.b_default;
	Log.set_show_time true

let version = VERSION

let usage = String.concat "\n" [
  "Usage:";
  "  * dep2pict <options> -dep <dep_file> -o <output_file>";
  "  * dep2pict <options> -conll <<conll_file> -o <output_file>";
  "Options:";
  "  -png: set output format to png (this is the default)";
  "  -pdf: set output format to pdf";
  "  -svg: set output format to svg";
  "  -features <string>: set features to display with CONLL input, string should contain a subset of {l,p,s,t,g,m,n} or A (for all). Default is \"\"";
  "  -ref: add dotted links for ellpsis";
  "  -v: display version number ("^version^")";
]

type output = Png | Svg | Pdf
let output = ref Png

type input = Dep | Conll | Xml of int 
let input = ref Dep

let debug = ref false

let input_file = ref None
let output_file = ref None

let eps_ref = ref false
let conll_features = ref ""

let _ =
  if Array.length Sys.argv = 1
  then (IFDEF BUILD_GUI THEN Gui.main () ELSE Log.critical "Gui not available" END)
  else
    begin
      let rec opt = function 
	| [] -> ()
	| "-v"::_ -> printf "%s\n%!" version; exit 0
        | "-help"::_ -> printf "%s\n%!" usage; exit 0

	| "-conll"::file::tail -> input_file := Some file; input := Conll; opt tail
	| "-dep"::file::tail -> input_file := Some file; opt tail

        (* NB: the xml option is used by parconine: arg is an integer, the xml_file is given with the -dep option *)
	| "-xml"::n::tail -> input := Xml (int_of_string n); opt tail
   
	| "-o"::file::tail -> output_file := Some file; opt tail
	| "-png"::tail -> output := Png; opt tail
	| "-svg"::tail -> output := Svg; opt tail
	| "-pdf"::tail -> output := Pdf; opt tail

	| "-features"::feats::tail -> conll_features := feats; opt tail
        | "-ref"::tail -> eps_ref := true; opt tail

        | "-d"::tail -> debug := true; opt tail

	| others::tail -> Log.fcritical "%s : option unknown!\n%s" others usage
      in opt (List.tl (Array.to_list Sys.argv));

      match (!input_file, !output_file) with
      | (None,_) -> Log.critical "No input file."
      | (Some input,_) when not (Sys.file_exists input) -> Log.fcritical "The file %s doesn't exist." input
      | (_,None) -> Log.critical "No ouput file specified."
      | (Some in_file, Some out_file) ->
          try
            (match (!input, !output) with
            | (Xml i, Png) -> ignore (Dep2pict.fromXmlFileToPng in_file out_file i)
            | (Dep, Png) -> ignore (Dep2pict.fromDepFileToPng in_file out_file)
            | (Conll, Png) -> ignore (Dep2pict.fromConllFileToPng ~features:!conll_features ~eps_ref:!eps_ref in_file out_file)

            | (Xml i, Svg) -> ignore (Dep2pict.fromXmlFileToSvgFile ~debug:(!debug) in_file out_file i)
            | (Dep, Svg) -> ignore (Dep2pict.fromDepFileToSvgFile ~debug:(!debug) in_file out_file)
            | (Conll, Svg) -> ignore (Dep2pict.fromConllFileToSvgFile ~debug:(!debug) ~features:!conll_features ~eps_ref:!eps_ref in_file out_file)

            | (Xml i, Pdf) -> ignore (Dep2pict.fromXmlFileToPdf in_file out_file i)
            | (Dep, Pdf) -> ignore (Dep2pict.fromDepFileToPdf in_file out_file)
            | (Conll, Pdf) -> ignore (Dep2pict.fromConllFileToPdf ~features:!conll_features ~eps_ref:!eps_ref in_file out_file));

              Log.finfo "File %s generated." out_file
          with
	  | Dep2pict.Parse_error msgs -> List.iter (fun (l,m) -> printf "Line %d: %s\n" l m) msgs; Log.fcritical "Parse error"
	  | Dep2pict.Id_already_in_use_ id -> Log.fcritical "Id already in use : %s" id
	  | Dep2pict.Unknown_index id -> Log.fcritical "Can't find index : %s" id
	  | Dep2pict.Loop_in_dep msg -> Log.fcritical "Loop in dependency : %s" msg
    end
