open Printf
open Log

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
let string_of_format = function
  | Dep -> "dep"
  | Conll -> "conll"
  | Xml -> "xml"
  | Png -> "png"
  | Svg -> "svg"
  | Pdf -> "pdf"



let (input_file : string option ref) = ref None
let (output_file : string option ref) = ref None

let current_infos = ref ["pos"]

(* elements of [current array] are pairs (sentid, code) *)
let (current_array : (string * string) array ref) = ref [||]
let (current_position : int option ref) = ref None   (* position of the current_focus in the current_array *)

let current_source = ref ""
let modified = ref false

let debug = ref false

let get_pos () = match !current_position with
  | None -> 0
  | Some v -> v

let get_id () = fst (!current_array.(get_pos ()))

(* -------------------------------------------------------------------------------- *)
let array_assoc key array =
  let len = Array.length array in
  let rec loop i =
    if i = len
    then raise Not_found
    else
      match array.(i) with
        | (k,v) when k=key -> (i,v)
        | _ -> loop (i+1)
  in loop 0


(* -------------------------------------------------------------------------------- *)
let search_sentid sentid =
  let (new_pos,_) = array_assoc sentid !current_array in
  current_position := Some new_pos

(* -------------------------------------------------------------------------------- *)
let update_source () =
  match !current_position with
    | None -> ()
    | Some p -> current_source := (snd !current_array.(p))

(* -------------------------------------------------------------------------------- *)
let first () =
  current_position := Some 0;
  update_source ()

(* -------------------------------------------------------------------------------- *)
let last () =
  current_position := Some ((Array.length !current_array) - 1);
  update_source ()

(* -------------------------------------------------------------------------------- *)
let next () = 
  match !current_position with
    | Some p when p < (Array.length !current_array)-1 ->
      current_position := Some (p+1);
      update_source ()
    | x -> ()

(* -------------------------------------------------------------------------------- *)
let has_next () = 
  match !current_position with
    | Some p when p < (Array.length !current_array)-1 -> true
    | _ -> false

(* -------------------------------------------------------------------------------- *)
let prev () = 
  match !current_position with
    | Some p when p > 0 ->
      current_position := Some (p-1);
      update_source ()
    | x -> ()

(* -------------------------------------------------------------------------------- *)
let has_prev () = 
  match !current_position with
    | Some p when p > 0 -> true
    | _ -> false

(* -------------------------------------------------------------------------------- *)
let view_label () =
  match (!current_array, !current_position) with
    | ([||], None) -> "No dep"
    | ([|(id,_)|], Some 0) when id="00001" -> "no_id"
    | ([|(id,_)|], Some 0) -> sprintf "[%s]" id
    | (a, Some p) ->
      let (id, _) = a.(p) in
      if id = sprintf "%05d" p
      then sprintf "(%d/%d) [no_id]" (p+1) (Array.length a)
      else sprintf "(%d/%d) [%s]" (p+1) (Array.length a) id
    | _ -> Log.critical "Inconsistent state"

(* -------------------------------------------------------------------------------- *)
let file_label () =
  match !input_file with
    | None -> "No file loaded"
    | Some in_file -> in_file

(* -------------------------------------------------------------------------------- *)
let write file string =
  let out_ch = open_out file in
  fprintf out_ch "%s\n" string;
  close_out out_ch

(* -------------------------------------------------------------------------------- *)
let save file =
  match !current_position with
  | Some index -> !current_array.(index) <- (fst (!current_array.(index)), !current_source);
  let out_ch = open_out file in
  Array.iter (fun (_,src) -> fprintf out_ch "%s\n" src) !current_array;
  close_out out_ch;
  modified := false

(* -------------------------------------------------------------------------------- *)
let load file =
  let in_ch = open_in file in
  let buff = Buffer.create 32 in
  let cpt = ref 0 in
  let sentid = ref None in
  let current_list = ref [] in
  try
    while true do
      let line = input_line in_ch in
      match (!sentid, line) with
        | None, "" -> ()
        | Some si, "" -> 
          current_list := (si, Buffer.contents buff) :: !current_list;
          Buffer.clear buff;
          sentid := None;
        | Some oc, line -> Printf.bprintf buff "%s\n" line
        | None, line ->
          incr cpt;
          let new_sentid = 
            match Str.split (Str.regexp "\t") line with
              | [_;_;_;_;_;"_";_;_;_;_] -> sprintf "%05d.conll" !cpt
              | [_;_;_;_;_;fs_string;_;_;_;_] ->
                let fs = List.map
                  (fun feat_string ->
                    match Str.split (Str.regexp "=") feat_string with
                      | [name;value] -> (name,value)
                      | _ -> failwith (Printf.sprintf "#1 >>%S<<\n%!" feat_string)
                  ) (Str.split (Str.regexp "|") fs_string) in
                (try List.assoc "sentid" fs with Not_found -> sprintf "%05d.conll" !cpt)
              | _ -> sprintf "%05d.conll" !cpt in
          sentid := Some new_sentid;
          Printf.bprintf buff "%s\n" line
    done
  with End_of_file ->
    (match !sentid with
      | Some si -> current_list := (si, Buffer.contents buff) :: !current_list
      | None -> ());
    current_array := Array.of_list (List.rev !current_list)

