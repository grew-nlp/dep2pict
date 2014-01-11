open Printf
open Dep2pict
open Ui

open Rsvg
open GMain

let (input_file : string option ref) = ref None
let (output_file : string option ref) = ref None

let refresh_svg (ui:ui) () =
  let input_text =
    ui#dep_source#buffer#get_text 
      ~start:ui#dep_source#buffer#start_iter 
      ~stop:ui#dep_source#buffer#end_iter 
      ~slice:true 
      ~visible:true () in
  try
    let svg =
      if input_text <> "" && input_text.[0] = '1'
      then Dep2pict.fromConllStringToSvgString input_text
      else Dep2pict.fromDepStringToSvgString input_text in
    let pixbuf = Rsvg.render_from_string ~size_cb:(Rsvg.at_zoom 1.0 1.0) svg in
    ui#svg_view#set_pixbuf pixbuf;
    ui#error_view#buffer#set_text "";
  with
    | Dep2pict.Parse_error msgs ->
      ui#error_view#buffer#set_text
        (String.concat "\n" (List.map (fun (l,m) -> sprintf "Line %d: %s" l m) msgs));
    | Dep2pict.Id_already_in_use_ id -> ui#error_view#buffer#set_text ("Id already in use : "^id)
    | Dep2pict.Unknown_index id -> ui#error_view#buffer#set_text ("Can't find index : "^id)
    | Dep2pict.Loop_in_dep msg -> ui#error_view#buffer#set_text ("Loop in dependency : "^msg)
    | Dep2pict.Conll_format msg -> ui#error_view#buffer#set_text ("Conll format : "^msg)

let writeFile path str =
  let file = open_out path in
  output file str 0 (String.length str);
  flush file;
  close_out file
 	
let readFile path =
  let text =
    let file = open_in path in
    let fileSize = in_channel_length file in
    let buffer = String.create fileSize in
    really_input file buffer 0 fileSize;
    close_in file;
    buffer in text

let save (ui:ui) () =
	let compute file () =
		writeFile file#filename (ui#dep_source#buffer#get_text 
			~start:ui#dep_source#buffer#start_iter 
			~stop:ui#dep_source#buffer#end_iter 
			~slice:true 
			~visible:true ())
	in
	let sel = GWindow.file_selection ~title:"Save file" ~show:true() in
	ignore(sel#ok_button#connect#clicked ~callback:(compute sel));
	ignore(sel#ok_button#connect#clicked ~callback:(sel#destroy));
	ignore(sel#cancel_button#connect#clicked ~callback:(sel#destroy));
	sel#complete ~filter:"*.dep";
	()

let open_dep (ui:ui) () =
  let compute file () =
    ui#dep_source#buffer#set_text (readFile file#filename);
    input_file := Some file#filename in
  let sel = GWindow.file_selection ~title:"Open file" ~show:true() in
  ignore(sel#ok_button#connect#clicked ~callback:(compute sel));
  ignore(sel#ok_button#connect#clicked ~callback:(sel#destroy));
  ignore(sel#cancel_button#connect#clicked ~callback:(sel#destroy));
  sel#complete ~filter:"*.dep";
  ()


let reload (ui:ui) () =
  match !input_file with
    | Some filename -> ui#dep_source#buffer#set_text (readFile filename)
    | None -> ()

let to_svg (ui:ui) () =
	let compute file () =
		writeFile file#filename (Dep2pict.fromDepStringToSvgString (ui#dep_source#buffer#get_text 
			~start:ui#dep_source#buffer#start_iter 
			~stop:ui#dep_source#buffer#end_iter 
			~slice:true 
			~visible:true ()))
	in
	let sel = GWindow.file_selection ~title:"Save file as svg" ~show:true() in
	ignore(sel#ok_button#connect#clicked ~callback:(compute sel));
	ignore(sel#ok_button#connect#clicked ~callback:(sel#destroy));
	ignore(sel#cancel_button#connect#clicked ~callback:(sel#destroy));
	sel#complete ~filter:"*.svg";
	()
	
let to_png (ui:ui) () =
	let compute file () =
		ignore(Dep2pict.fromDepStringToPng (ui#dep_source#buffer#get_text 
			~start:ui#dep_source#buffer#start_iter 
			~stop:ui#dep_source#buffer#end_iter 
			~slice:true 
			~visible:true ()) file#filename)
	in
	let sel = GWindow.file_selection ~title:"Save file as png" ~show:true() in
	ignore(sel#ok_button#connect#clicked ~callback:(compute sel));
	ignore(sel#ok_button#connect#clicked ~callback:(sel#destroy));
	ignore(sel#cancel_button#connect#clicked ~callback:(sel#destroy));
	sel#complete ~filter:"*.png";
	()
	
let to_pdf (ui:ui) () =
	let compute file () =
		ignore(Dep2pict.fromDepStringToPdf (ui#dep_source#buffer#get_text 
			~start:ui#dep_source#buffer#start_iter 
			~stop:ui#dep_source#buffer#end_iter 
			~slice:true 
			~visible:true ()) file#filename)
	in
	let sel = GWindow.file_selection ~title:"Save file as pdf" ~show:true() in
	ignore(sel#ok_button#connect#clicked ~callback:(compute sel));
	ignore(sel#ok_button#connect#clicked ~callback:(sel#destroy));
	ignore(sel#cancel_button#connect#clicked ~callback:(sel#destroy));
	sel#complete ~filter:"*.pdf";
	()

let main ?file () = 
  ignore (GMain.Main.init ());
  let ui = new ui () in 
  ui#toplevel#set_title ("Dep2pict v. "^VERSION);
	
	
  ignore(ui#toplevel#connect#destroy ~callback:GMain.Main.quit);
  ignore(ui#save#connect#clicked ~callback:(save ui));
  ignore(ui#open_btn#connect#clicked ~callback:(open_dep ui));
  ignore(ui#reload#connect#clicked ~callback:(reload ui));
  ignore(ui#to_svg#connect#clicked ~callback:(to_svg ui));
  ignore(ui#to_png#connect#clicked ~callback:(to_png ui));
  ignore(ui#to_pdf#connect#clicked ~callback:(to_pdf ui));

  (match file with
    | None -> ()
    | Some f -> ui#dep_source#buffer#set_text (readFile f)
  );

  refresh_svg ui ();

  ignore(ui#dep_source#buffer#connect#changed ~callback:(refresh_svg ui));
	
  ui#check_widgets ();
  ui#toplevel#show ();
  GMain.Main.main ()
