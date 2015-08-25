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
let all_files () =
  GFile.filter
    ~name:"All files"
    ~patterns:[ "*" ] ()

let dep_filter () =
  GFile.filter
    ~name:"Dependency structure"
    ~patterns:[ "*.dep"; "*.conll" ] ()

let check_modified continuation () =
  if not !modified
  then continuation ()
  else
    let md = GWindow.message_dialog
      ~message:("You have unsaved changes in "^(get_id ())^"\nDo you really want to discard your changes")
      ~buttons: GWindow.Buttons.ok_cancel
      ~modal: true
      ~urgency_hint:true
      ~message_type:`QUESTION
      () in
  let _ = md#connect#response 
    (function
       | `CANCEL | `DELETE_EVENT -> md#destroy ()
       | `OK -> md#destroy (); modified := false; continuation ()
    ) in
  md#show ()


(* -------------------------------------------------------------------------------- *)
let main () =

  let _ = GMain.Main.init () in

  (* Build the main windows *)
  let ui = new ui () in
  let _ = ui#toplevel#connect#destroy ~callback:GMain.Main.quit in

  (* Build the webkit widget *)
  let webkit = GWebView.web_view ~show:true ~packing:ui#scroll#add () in
  let _ = webkit#set_full_content_zoom true in

  (* -------------------------------------------------------------------------------- *)
  let refresh_view () =
    try
      let graph =
        if !current_source <> "" && !current_source.[0] = '1'
        then Dep2pict.from_conll !current_source
        else Dep2pict.from_dep !current_source in
 
     let svg = try Dep2pict.to_svg graph with _ -> failwith "FFF" in
      webkit#load_html_string svg "";
      webkit#set_zoom_level (!current_zoom /. 100.);

      ui#error_view#buffer#set_text "";

      let _ = ui#prev_button#misc#set_sensitive (has_prev ()) in
      let _ = ui#next_button#misc#set_sensitive (has_next ()) in
      let _ = ui#first_button#misc#set_sensitive (has_prev ()) in
      let _ = ui#last_button#misc#set_sensitive (has_next ()) in
      ui#view_label#set_text (view_label ());
      ui#toplevel#set_title (file_label ());

    with
      | Dep2pict.Parse_error msgs ->
        ui#error_view#buffer#set_text
          (String.concat "\n" (List.map (fun (l,m) -> sprintf "Line %d: %s" l m) msgs));
      | Dep2pict.Id_already_in_use_ id -> ui#error_view#buffer#set_text ("Id already in use : "^id)
      | Dep2pict.Unknown_index id -> ui#error_view#buffer#set_text ("Can't find index : "^id)
      | Dep2pict.Loop_in_dep msg -> ui#error_view#buffer#set_text ("Loop in dependency : "^msg)
      | Dep2pict.Conll_format msg -> ui#error_view#buffer#set_text ("Conll format : "^msg) in

  (* -------------------------------------------------------------------------------- *)
  let reload () =
    match !input_file with
      | Some filename ->
        load filename;
        update_source ();
        refresh_view ()
      | None -> () in

  (* -------------------------------------------------------------------------------- *)
  let open_dep () =
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
          reload ()
        | `DELETE_EVENT | `CANCEL -> ()
    end ;
    dialog#destroy () in

  (* -------------------------------------------------------------------------------- *)
  let open_editor () =
    let editor = new editor () in
    editor#source#buffer#set_text !current_source;
    modified := false;

    ignore(editor#toplevel#connect#destroy ~callback:editor#toplevel#destroy);
    ignore(editor#close_button#connect#clicked ~callback:editor#toplevel#destroy);

    let refresh_current_source () =
      modified := true;
      current_source :=
        editor#source#buffer#get_text
        ~start:editor#source#buffer#start_iter
        ~stop:editor#source#buffer#end_iter
        ~slice:true
        ~visible:true () in

    ignore(editor#source#buffer#connect#changed
             ~callback:(fun () -> refresh_current_source (); refresh_view ()));

    editor#check_widgets ();
    editor#toplevel#show ();
    () in

  (* -------------------------------------------------------------------------------- *)
  let save_as () =
    let sel = GWindow.file_selection ~title:"Save file" ~show:true() in
    (* TODO deal with dep|conll... sel#complete ~filter:"*.conll"; *)

    let _ = sel#ok_button#connect#clicked
      ~callback: (fun () ->
        save sel#filename;
        input_file := Some sel#filename;
        sel#destroy ()
      ) in

    let _ = sel#cancel_button#connect#clicked ~callback:(sel#destroy) in
    () in

  (* -------------------------------------------------------------------------------- *)
  let save () =
    match !input_file with
      | Some file -> save file
      | None -> save_as () in

  (* -------------------------------------------------------------------------------- *)
  let convert () =

    let format =
      match (ui#svg_radio#active, ui#png_radio#active, ui#pdf_radio#active) with
        | (true,false,false) -> Format.Svg
        | (false,true,false) -> Format.Png
        | (false,false,true) -> Format.Pdf
        | _ -> Log.critical "Inconsistent format radiobuttons" in

    let title = sprintf "Convert file to %s" (Format.to_string format) in
    let file_window = GWindow.file_selection ~title ~show:true() in
    let _ = file_window#ok_button#connect#clicked
      ~callback:(fun () ->
        begin
          let graph =
            if String.length !current_source > 0 && !current_source.[0] = '1'
            then Dep2pict.from_conll !current_source
            else Dep2pict.from_dep !current_source in

          match format with
            | Format.Svg -> Dep2pict.save_svg ~filename:file_window#filename graph
            | Format.Png -> Dep2pict.save_png ~filename:file_window#filename graph
            | Format.Pdf -> Dep2pict.save_pdf ~filename:file_window#filename graph
            | _ -> Log.fcritical "Unsupported output format: %s" (Format.to_string format)
        end;
        file_window#destroy ()
      ) in
    let _ =  file_window#cancel_button#connect#clicked ~callback:(file_window#destroy) in
    let _ = file_window#complete ~filter:("*."^(Format.to_string format)) in
    () in


  let _ = ui#reload#connect#clicked ~callback:reload in
  let _ = GMisc.image ~stock:`REFRESH ~packing: ui#reload_box#pack () in

  let _ = ui#save#connect#clicked ~callback:save in
  let _ = GMisc.image ~stock:`SAVE ~packing:ui#save_box#pack () in

  let _ = ui#save_as#connect#clicked ~callback:save_as in
  let _ = GMisc.image ~stock:`SAVE_AS ~packing: ui#save_as_box#pack () in

  let _ = ui#edit#connect#clicked ~callback:open_editor in
  let _ = GMisc.image ~stock:`EDIT ~packing: ui#edit_box#pack () in

  let _ = ui#open_button#connect#clicked ~callback:open_dep in
  let _ = GMisc.image ~stock:`OPEN ~packing: ui#open_button_box#pack () in

  let _ = ui#first_button#connect#clicked
    ~callback: (fun () -> check_modified (fun () -> first (); refresh_view ()) ()) in
  let _ = GMisc.image ~stock:`GOTO_FIRST ~packing: ui#first_button_box#pack () in

  let _ = ui#prev_button#connect#clicked
    ~callback: (fun () -> check_modified (fun () -> prev (); refresh_view ()) ()) in
  let _ = GMisc.image ~stock:`GO_BACK ~packing: ui#prev_button_box#pack () in

  let _ = ui#next_button#connect#clicked
    ~callback: (fun () -> check_modified (fun () -> next (); refresh_view ()) ()) in
  let _ = GMisc.image ~stock:`GO_FORWARD ~packing: ui#next_button_box#pack () in

  let _ = ui#last_button#connect#clicked
    ~callback: (fun () -> check_modified (fun () -> last (); refresh_view ()) ()) in
  let _ = GMisc.image ~stock:`GOTO_LAST ~packing: ui#last_button_box#pack () in

  let _ = ui#convert_button#connect#clicked ~callback:convert in
  let _ = GMisc.image ~stock:`OPEN ~packing: ui#convert_button_box#pack () in

  let _ = ui#zoom#connect#value_changed
    ~callback:
    (fun () ->
      current_zoom := ui#zoom#adjustment#value;
      webkit#set_zoom_level (!current_zoom /. 100.);
    ) in

  let _ = ui#toplevel#event#connect#key_press
    ~callback: (fun ev ->
      let key = GdkEvent.Key.keyval ev in
      let modif = GdkEvent.Key.state ev in
      begin
        match modif with
          | [`CONTROL] when key = _r -> reload ()
          | [`CONTROL] when key = _s -> save ()
          | [`CONTROL] when key = _e -> open_editor ()
          | [`CONTROL] when key = _o -> open_dep ()

          | [] when key = _Left ->
            if has_prev () then check_modified (fun () -> prev (); refresh_view ()) ()
          | [] when key = _Right ->
            if has_next () then check_modified (fun () -> next (); refresh_view ()) ()
          | [`CONTROL] when key = _Left ->
            if has_prev () then check_modified (fun () -> first (); refresh_view ()) ()
          | [`CONTROL] when key = _Right ->
            if has_next () then check_modified (fun () -> last (); refresh_view ()) ()

          | [`CONTROL] when key = _c -> convert ()

          | _ -> ()
      end;
      true) in

  ui#check_widgets ();
  ui#toplevel#show ();
  update_source ();
  refresh_view ();

  GMain.Main.main ()
