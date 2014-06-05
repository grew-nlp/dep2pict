open Printf
open Log

open Rsvg
open GMain
open GdkKeysyms

open Dep2pict
open Dep2pict_glade

open Global

(* -------------------------------------------------------------------------------- *)
let current_zoom = ref 100.0



  
(* -------------------------------------------------------------------------------- *)
let refresh_view (ui:ui) () =
  try
    let svg =
      if !current_source <> "" && !current_source.[0] = '1'
      then Dep2pict.fromConllStringToSvgString ~infos:(!current_infos) !current_source
      else Dep2pict.fromDepStringToSvgString !current_source in
    let pixbuf = Rsvg.render_from_string ~size_cb:(Rsvg.at_zoom (!current_zoom /. 100.) (!current_zoom /. 100.)) svg in
    ui#svg_view#set_pixbuf pixbuf;
    ui#error_view#buffer#set_text "";
    let _ = ui#prev_button#misc#set_sensitive (has_prev ()) in
    let _ = ui#next_button#misc#set_sensitive (has_next ()) in
    let _ = ui#first_button#misc#set_sensitive (has_more_than_one ()) in
    let _ = ui#last_button#misc#set_sensitive (has_more_than_one ()) in
    ui#view_label#set_text (view_label ());
    ui#toplevel#set_title (file_label ());
     

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
      save sel#filename;
      input_file := Some sel#filename;
      sel#destroy ()
    ) in

  let _ = sel#cancel_button#connect#clicked ~callback:(sel#destroy) in
  ()

(* -------------------------------------------------------------------------------- *)
let save (ui:ui) () =
  match !input_file with
    | Some file -> save file
    | None -> save_as (ui:ui) ()

(* -------------------------------------------------------------------------------- *)
let reload (ui:ui) () =
  match !input_file with
    | Some filename ->
      load filename;
      update_source ();
      refresh_view ui ()
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
        input_file := dialog#filename;
        reload ui ()
      | `DELETE_EVENT | `CANCEL -> ()
  end ;
  dialog#destroy ()

(* -------------------------------------------------------------------------------- *)
let convert (ui:ui) () =

  let format = 
    match (ui#svg_radio#active, ui#png_radio#active, ui#pdf_radio#active) with
      | (true,false,false) -> Svg
      | (false,true,false) -> Png
      | (false,false,true) -> Pdf
      | _ -> Log.critical "Inconsistent format radiobuttons" in

  let title = sprintf "Convert file to %s" (string_of_format format) in
  let file_window = GWindow.file_selection ~title ~show:true() in
  let _ = file_window#ok_button#connect#clicked
    ~callback:(fun () -> 
      begin
        let dep_code =
          if String.length !current_source > 0 && !current_source.[0] = '1'
          then Dep2pict.fromConllStringToDepString !current_source
          else !current_source in
        match format with
          | Svg -> write file_window#filename (Dep2pict.fromDepStringToSvgString dep_code)
          | Png -> ignore (Dep2pict.fromDepStringToPng dep_code file_window#filename)
          | Pdf -> ignore (Dep2pict.fromDepStringToPdf dep_code file_window#filename)
          | _ -> Log.fcritical "Unsupported output format: %s" (string_of_format format)
      end;
      file_window#destroy ()
    ) in
  let _ =  file_window#cancel_button#connect#clicked ~callback:(file_window#destroy) in
  let _ = file_window#complete ~filter:("*."^(string_of_format format)) in
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
           ~callback:(fun () -> refresh_current_source (); refresh_view parent ()));

  editor#check_widgets ();
  editor#toplevel#show ();
  ()

(* -------------------------------------------------------------------------------- *)
let main () =
  let _ = GMain.Main.init () in
  let ui = new ui () in
  let _ = ui#toplevel#connect#destroy ~callback:GMain.Main.quit in

  let _ = ui#reload#connect#clicked ~callback:(reload ui) in
  let _ = GMisc.image ~stock:`REFRESH ~packing: ui#reload_box#pack () in

  let _ = ui#save#connect#clicked ~callback:(save ui) in
  let _ = GMisc.image ~stock:`SAVE ~packing: ui#save_box#pack () in

  let _ = ui#save_as#connect#clicked ~callback:(save_as ui) in
  let _ = GMisc.image ~stock:`SAVE_AS ~packing: ui#save_as_box#pack () in

  let _ = ui#edit#connect#clicked ~callback:(open_editor ui) in
  let _ = GMisc.image ~stock:`EDIT ~packing: ui#edit_box#pack () in

  let _ = ui#open_button#connect#clicked ~callback:(open_dep ui) in
  let _ = GMisc.image ~stock:`OPEN ~packing: ui#open_button_box#pack () in

  let _ = ui#first_button#connect#clicked ~callback: (fun () -> first (); refresh_view ui ()) in
  let _ = GMisc.image ~stock:`GOTO_FIRST ~packing: ui#first_button_box#pack () in

  let _ = ui#prev_button#connect#clicked ~callback: (fun () -> prev (); refresh_view ui ()) in
  let _ = GMisc.image ~stock:`GO_BACK ~packing: ui#prev_button_box#pack () in

  let _ = ui#next_button#connect#clicked ~callback: (fun () -> next (); refresh_view ui ()) in
  let _ = GMisc.image ~stock:`GO_FORWARD ~packing: ui#next_button_box#pack () in

  let _ = ui#last_button#connect#clicked ~callback: (fun () -> last (); refresh_view ui ()) in
  let _ = GMisc.image ~stock:`GOTO_LAST ~packing: ui#last_button_box#pack () in

  let _ = ui#convert_button#connect#clicked ~callback:(convert ui) in
  let _ = GMisc.image ~stock:`CONVERT ~packing: ui#convert_button_box#pack () in

  let _ = ui#zoom#connect#value_changed
      ~callback: (fun () -> current_zoom := ui#zoom#adjustment#value; refresh_view ui ()) in

  let _ = ui#toplevel#event#connect#key_press
    ~callback: (fun ev -> 
      let key = GdkEvent.Key.keyval ev in
      let modif = GdkEvent.Key.state ev in
      begin
        match modif with
          | [`CONTROL] when key = _r -> reload ui ()
          | [`CONTROL] when key = _s -> open_dep ui ()
          | [`CONTROL] when key = _e -> open_editor ui ()
          | [`CONTROL] when key = _o -> open_dep ui ()

          | [] when key = _Left -> prev (); refresh_view ui ()
          | [] when key = _Right -> next (); refresh_view ui ()
          | [`CONTROL] when key = _Left -> first (); refresh_view ui ()
          | [`CONTROL] when key = _Right -> last (); refresh_view ui ()

          | [`CONTROL] when key = _c -> convert ui ()

          | _ -> ()
      end;
      true) in

  ui#check_widgets ();
  ui#toplevel#show ();
  update_source ();
  refresh_view ui ();
  GMain.Main.main ()
