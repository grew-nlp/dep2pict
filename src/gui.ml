open Printf
open Log
open Rsvg
open GMain
open GdkKeysyms

open Conll

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
    let graph = match (!current_data, !current_position) with
      | (Conll arr, p) -> Dep2pict.from_conll (snd arr.(p))
      | (Dep graph, _) -> graph in

    let svg = Dep2pict.to_svg graph in
      webkit#load_html_string svg "";
      webkit#set_zoom_level (!current_zoom /. 100.);

      ui#error_view#buffer#set_text "";

      let _ = ui#prev_button#misc#set_sensitive (has_prev ()) in
      let _ = ui#next_button#misc#set_sensitive (has_next ()) in
      let _ = ui#first_button#misc#set_sensitive (has_prev ()) in
      let _ = ui#last_button#misc#set_sensitive (has_next ()) in
      ui#view_label#set_text (view_label ());
      ui#toplevel#set_title !input_file;
      () in

  (* -------------------------------------------------------------------------------- *)
  (* Hack to keep the horizontal position *)
  let user_hpos = ref 0. in
  let _ = GMain.Timeout.add ~ms:50
    ~callback: (fun () ->
      if ui#scroll#hadjustment#value = 0. && !user_hpos > 0.
      then (ui#scroll#hadjustment#set_value !user_hpos; user_hpos := 0.);
    true) in

  let reload first =
      try

    load !input_file;
    if first then set_position ();
    input_last_modifaction_time := (let stat = Unix.stat !input_file in stat.Unix.st_mtime);
    user_hpos := ui#scroll#hadjustment#value; (* Hack (cf above) *)
    refresh_view ()
    with
      | Dep2pict.Parse_error msgs ->
        ui#error_view#buffer#set_text
          (String.concat "\n" (List.map (fun (l,m) -> sprintf "Line %d: %s" l m) msgs));
      | Dep2pict.Id_already_in_use_ id -> ui#error_view#buffer#set_text ("Id already in use: "^id)
      | Dep2pict.Unknown_index id -> ui#error_view#buffer#set_text ("Can't find index: "^id)
      | Dep2pict.Loop_in_dep msg -> ui#error_view#buffer#set_text ("Loop in dependency: "^msg)
      | Dep2pict.Conll_format msg -> ui#error_view#buffer#set_text ("Conll format: "^msg)
      | Conll_types.Error json -> ui#error_view#buffer#set_text (Yojson.Basic.pretty_to_string json) in


  (* check if file has changed *)
  let _ = GMain.Timeout.add
    ~ms:1000
    ~callback:
      (fun () ->
        let stat = Unix.stat !input_file in
        if stat.Unix.st_mtime > !input_last_modifaction_time
        then reload false;
        true
     ) in

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
          (match dialog#filename with
          | Some f -> input_file := f; reload false;
          | None -> ())
        | `DELETE_EVENT | `CANCEL -> ()
    end ;
    dialog#destroy () in

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
          let graph = match (!current_data, !current_position) with
          | (Conll arr, p) -> Dep2pict.from_conll (snd arr.(p))
          | (Dep graph, _) -> graph in

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


  let _ = ui#reload#connect#clicked ~callback:(fun () -> reload false) in
  let _ = GMisc.image ~stock:`REFRESH ~packing: ui#reload_box#pack () in

  let _ = ui#open_button#connect#clicked ~callback:open_dep in
  let _ = GMisc.image ~stock:`OPEN ~packing: ui#open_button_box#pack () in

  let _ = ui#first_button#connect#clicked
    ~callback: (fun () -> first (); refresh_view ()) in
  let _ = GMisc.image ~stock:`GOTO_FIRST ~packing: ui#first_button_box#pack () in

  let _ = ui#prev_button#connect#clicked
    ~callback: (fun () -> prev (); refresh_view ()) in
  let _ = GMisc.image ~stock:`GO_BACK ~packing: ui#prev_button_box#pack () in

  let _ = ui#next_button#connect#clicked
    ~callback: (fun () -> next (); refresh_view ()) in
  let _ = GMisc.image ~stock:`GO_FORWARD ~packing: ui#next_button_box#pack () in

  let _ = ui#last_button#connect#clicked
    ~callback: (fun () -> last (); refresh_view ()) in
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
          | [`CONTROL] when key = _r -> reload false
          | [`CONTROL] when key = _o -> open_dep ()

          | [] when key = _Left ->
            if has_prev () then prev (); refresh_view ()
          | [] when key = _Right ->
            if has_next () then next (); refresh_view ()
          | [`CONTROL] when key = _Left ->
            if has_prev () then first (); refresh_view ()
          | [`CONTROL] when key = _Right ->
            if has_next () then last (); refresh_view ()

          | [`CONTROL] when key = _c -> convert ()

          | _ -> ()
      end;
      true) in

  ui#check_widgets ();
  ui#toplevel#show ();
  reload true;

  GMain.Main.main ()
