open Printf
open Dep2pict
open Ui

open Rsvg
open GMain

(* -------------------------------------------------------------------------------- *)
let writeFile path str =
  let file = open_out path in
  output file str 0 (String.length str);
  flush file;
  close_out file
 	
(* -------------------------------------------------------------------------------- *)
let readFile path =
  let text =
    let file = open_in path in
    let fileSize = in_channel_length file in
    let buffer = String.create fileSize in
    really_input file buffer 0 fileSize;
    close_in file;
    buffer in text


(* -------------------------------------------------------------------------------- *)
let (current_file : string option ref) = ref None
let current_infos = ref ["pos"]
let current_source = ref ""
let current_zoom = ref 100.0

(* -------------------------------------------------------------------------------- *)
let refresh_svg (ui:ui) () =
  try
    let svg =
      if !current_source <> "" && !current_source.[0] = '1'
      then Dep2pict.fromConllStringToSvgString ~infos:(!current_infos) !current_source
      else Dep2pict.fromDepStringToSvgString !current_source in
    let pixbuf = Rsvg.render_from_string ~size_cb:(Rsvg.at_zoom (!current_zoom /. 100.) (!current_zoom /. 100.)) svg in
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


(* -------------------------------------------------------------------------------- *)
let save_as (ui:ui) () =
  let sel = GWindow.file_selection ~title:"Save file" ~show:true() in
  (* TODO deal with dep|conll... sel#complete ~filter:"*.conll"; *)

  let _ = sel#ok_button#connect#clicked
    ~callback: (fun () ->
      writeFile sel#filename !current_source;
      current_file := Some sel#filename;
      sel#destroy ()
    ) in

  let _ = sel#cancel_button#connect#clicked ~callback:(sel#destroy) in
  ()

(* -------------------------------------------------------------------------------- *)
let save (ui:ui) () =
  match !current_file with
    | Some file -> writeFile file !current_source
    | None -> save_as (ui:ui) ()

(* -------------------------------------------------------------------------------- *)
let reload (ui:ui) () =
  match !current_file with
    | Some filename ->
      current_source := readFile filename;
      refresh_svg ui ()
    | None -> ()

(* -------------------------------------------------------------------------------- *)
let all_files () =
  GFile.filter
    ~name:"All files"
    ~patterns:[ "*" ] ()

let dep_filter () = 
  GFile.filter 
    ~name:"Dependency structure" 
    ~patterns:[ "*.dep"; "*.conll" ] ()

let open_dep (ui:ui) () =
  let dialog = GWindow.file_chooser_dialog
    ~action:`OPEN
    ~title:"Open File"
    () in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
  dialog#add_filter (dep_filter ()) ;
  dialog#add_filter (all_files ());
  begin
    match dialog#run () with
      | `OPEN ->
        current_file := dialog#filename;
        reload ui ()
      | `DELETE_EVENT | `CANCEL -> ()
  end ;
  dialog#destroy ()

(* -------------------------------------------------------------------------------- *)
let export format (ui:ui) () =
  let title = sprintf "Save file as %s" format in
  let file_window = GWindow.file_selection ~title ~show:true() in
  let _ = file_window#ok_button#connect#clicked
    ~callback:(fun () -> 
      begin
        let dep_code =
          if String.length !current_source > 0 && !current_source.[0] = '1'
          then Dep2pict.fromConllStringToDepString !current_source
          else !current_source in
        match format with
          | "svg" -> writeFile file_window#filename (Dep2pict.fromDepStringToSvgString dep_code)
          | "png" -> ignore (Dep2pict.fromDepStringToPng dep_code file_window#filename)
          | "pdf" -> ignore (Dep2pict.fromDepStringToPdf dep_code file_window#filename)
          | _ -> failwith (sprintf "Unknown format %s" format)
      end;
      file_window#destroy ()
    ) in
  let _ =  file_window#cancel_button#connect#clicked ~callback:(file_window#destroy) in
  let _ = file_window#complete ~filter:("*."^format) in
  ()

(* -------------------------------------------------------------------------------- *)
let open_editor parent () =
  let editor = new editor () in
  editor#source#buffer#set_text !current_source;

  ignore(editor#toplevel#connect#destroy ~callback:editor#toplevel#destroy);
  ignore(editor#close_button#connect#clicked ~callback:editor#toplevel#destroy);

  let refresh_current_source () =
    current_source :=
      editor#source#buffer#get_text 
      ~start:editor#source#buffer#start_iter 
      ~stop:editor#source#buffer#end_iter 
      ~slice:true 
      ~visible:true () in

  ignore(editor#source#buffer#connect#changed
           ~callback:(fun () -> refresh_current_source (); refresh_svg parent ()));


  editor#check_widgets ();
  editor#toplevel#show ();
  ()

(* -------------------------------------------------------------------------------- *)
let main ?infos ?file () =
  let _ = GMain.Main.init () in
  let ui = new ui () in
  let () = ui#toplevel#set_title ("Dep2pict v. "^VERSION) in
  let _ = ui#toplevel#connect#destroy ~callback:GMain.Main.quit in
  let _ = ui#edit#connect#clicked ~callback:(open_editor ui) in
  let _ = ui#save#connect#clicked ~callback:(save ui) in
  let _ = ui#save_as#connect#clicked ~callback:(save_as ui) in
  let _ = ui#open_btn#connect#clicked ~callback:(open_dep ui) in
  let _ = ui#reload#connect#clicked ~callback:(reload ui) in

  let _ = ui#zoom#connect#value_changed
      ~callback:
      (fun () ->
        current_zoom := ui#zoom#adjustment#value; 
        refresh_svg ui ()
      ) in

  ignore(ui#to_svg#connect#clicked ~callback:(export "svg" ui));
  ignore(ui#to_png#connect#clicked ~callback:(export "png" ui));
  ignore(ui#to_pdf#connect#clicked ~callback:(export "pdf" ui));

  (match infos with
    | None -> ()
    | Some cst -> current_infos := cst
  );

  (match file with
    | None -> ()
    | Some f -> current_file := Some f; reload ui ()
  );

  ui#check_widgets ();
  ui#toplevel#show ();
  GMain.Main.main ()
