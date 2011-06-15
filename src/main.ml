open Dep2pict
open Log
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

let usage = "Usage : dep2pict -dep <dep_file> -o <output_file> [ -png | -svg | -pdf ]\n"
^"\t -dep <dep_file> : input file\n"
^"\t -xml <n> : input file is an xml file, want to the nth dep\n"
^"\t -o <file> : output file (without extension)\n"
^"\t -png : to transform dep_file to png file\n"
^"\t -pdf : to transform dep_file to png file\n"
^"\t -svg : to transform dep_file to svg file\n"
^"\t -v : display version number ("^VERSION^")\n";;

let input_file = ref "";;
let output_file = ref "";;
let png = ref false;;
let svg = ref false;;
let pdf = ref false;;
let xml = ref false;;
let dep = ref 1;;

let _ =
    if (Array.length Sys.argv > 1) then (
		let rec opt = function arg ->
		match arg with
				| "-v"::tail ->
					Printf.printf "%s\n%!" version;
					exit 0;
		        | "-dep"::file::tail -> 
		            input_file := file;
		            opt tail;
		            ()
		        | "-o"::file::tail -> 
		            output_file := file;
		            opt tail;
		            ()
		        | "-png"::tail -> 
		            png := true;
		            opt tail;
		            ()
		        | "-svg"::tail -> 
		            svg := true;
		            opt tail;
		            ()
		        | "-pdf"::tail -> 
		            pdf := true;
		            opt tail;
		            ()
		        | "-xml"::n::tail -> 
		            xml := true;
		            dep := int_of_string n;
		            opt tail;
		            ()
		        | [] -> ()
		        | others::tail -> Log.critical (Printf.sprintf "%s : option unknown!\n%s" others usage)
		in opt (List.tl (Array.to_list Sys.argv));
		if (Sys.file_exists !input_file) then (
			if ((not (!png)) && (not (!svg)) && (not (!pdf))) then (
				Log.info (Printf.sprintf "No picture generated!");
			) else (
				try
					if (!png) then (
						if (!xml) then (
							ignore(Dep2pict.fromXmlFileToPng (!input_file) (!output_file) !dep);
						) else (
							ignore(Dep2pict.fromDepFileToPng (!input_file) (!output_file));
						);
						Log.info (Printf.sprintf "File %s generated!" (!output_file));
					);
					if (!svg) then (
						if (!xml) then (
							ignore(Dep2pict.fromXmlFileToSvgFile (!input_file) (!output_file) !dep);
						) else (
							ignore(Dep2pict.fromDepFileToSvgFile (!input_file) (!output_file));
						);
						Log.info (Printf.sprintf "File %s generated!" (!output_file));
					);
					if (!pdf) then (
						if (!xml) then (
							ignore(Dep2pict.fromXmlFileToPdf (!input_file) (!output_file) !dep);
						) else (
							ignore(Dep2pict.fromDepFileToPdf (!input_file) (!output_file));
						);
						Log.info (Printf.sprintf "File %s generated!" (!output_file));
					)
				with
					| Dep2pict.Parse_error msgs ->
						let err = ref "" in
						List.iter ( fun (l,m) ->
							err := Printf.sprintf "%sLine %d : %s" !err l m
						) msgs;
						Printf.printf "%s" !err;
						exit 1;
					| Dep2pict.Id_already_in_use_ id -> Log.critical (Printf.sprintf "Id already in use : %s" id)
					| Dep2pict.Unknown_index id -> Log.critical (Printf.sprintf "Can't find index : %s" id)
					| Dep2pict.Loop_in_dep msg -> Log.critical (Printf.sprintf "Loop in dependency : %s" msg)
			)
		) else (
			if (!input_file<>"") then (
				Log.critical (Printf.sprintf "The file %s doesn't exist!" (!input_file))
			) else (
				Log.critical (Printf.sprintf "No input file!")
			)
		)
    ) else (
		IFDEF BUILD_GUI THEN Gui.main () END
    )
    
    
