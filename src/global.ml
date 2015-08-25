open Printf
open Log
open Dep2pict

module File = struct
  let read file =
    let in_ch = open_in file in
    (* if the input file contains an UTF-8 byte order mark (EF BB BF), skip 3 bytes, else get back to 0 *)
    (match input_byte in_ch with 0xEF -> seek_in in_ch 3 | _ -> seek_in in_ch 0);

    let buff = Buffer.create 32 in
    try
      while true do
        bprintf buff "%s\n" (input_line in_ch)
      done; assert false
    with End_of_file ->
      close_in in_ch;
      Buffer.contents buff

(* -------------------------------------------------------------------------------- *)
let write file string =
  let out_ch = open_out file in
  fprintf out_ch "%s\n" string;
  close_out out_ch

end (* module File *)


let batch = ref false

let critical msg = ksprintf
  (fun m -> match !batch  with
    | true -> printf "%s\n" m; exit 1
    | false -> Log.fcritical "%s" m
  ) msg


let logo = String.concat "\n" [
"[GRAPH] { word_spacing=0; opacity=50; scale=300 }";
"[WORDS] {";
"  A { word=\"Dep\"; forecolor=purple; }";
"  B { word=\"2\"; forecolor=orange; }";
"  C { word=\"pict\"; forecolor=pink; }";
"}";
"[EDGES] {";
"  A -> B { color=red; }";
"  C -> B { color=blue;}";
"  B -> A { bottom; color=yellow; }";
"  B -> C { bottom; color=green; }";
"}";
]



exception Found of int
  let get_suffix file_name =
  let len = String.length file_name in
    try
      for i = len-1 downto 0 do
        if file_name.[i] = '.'
        then raise (Found i)
      done;
      None
    with
    | Found i -> Some (String.sub file_name i (len-i))

module Format = struct
  type format =  Dep | Conll | Png | Svg | Pdf | No_suff | Unk of string

  let get file =
    match get_suffix file with 
    | None -> Log.fwarning "Cannot guess format (no suffix) for file \"%s\"" file; No_suff
    | Some suff -> 
      try List.assoc suff [ (".dep",Dep); (".conll",Conll); (".png",Png); (".svg",Svg); (".pdf",Pdf)]
      with Not_found -> Log.fwarning "Unkwnow file extension \"%s\" for file \"%s\"" suff file; Unk suff

  let to_string = function
    | Dep -> "dep"
    | Conll -> "conll"
    | Png -> "png"
    | Svg -> "svg"
    | Pdf -> "pdf"
    | No_suff -> "no_suff"
    | Unk suff -> sprintf "unkown suffix '%s'" suff
end

type input_data =
  | No_data
  | Dep of Dep2pict.t * string
  | Conll of (string * string) array

let (input_file : string option ref) = ref None
let (output_file : string option ref) = ref None

let current_data = ref No_data
let (current_position : int ref) = ref 0   (* position of the current_focus in the current_array *)
  
let current_source = ref logo (* by default use the logo code *)
let modified = ref false

let debug = ref false

let get_id () =
  match !current_data with
  | Conll arr -> fst (arr.(!current_position))
  | _ -> Log.critical "[get_id] can be use only with CONLL data"

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
  match !current_data with
  | Conll arr ->
    let (new_pos,_) = array_assoc sentid arr in current_position := new_pos
  | _ -> Log.critical "[search_sentid] can be use only with CONLL data"

(* -------------------------------------------------------------------------------- *)
let update_source () =
  match (!current_data, !current_position) with
    | (Conll arr, p) -> current_source := (snd arr.(p))
    | (Dep (graph,text), _) -> current_source := text
    | (No_data, _) -> current_source := logo

(* -------------------------------------------------------------------------------- *)
let first () =
  match !current_data with
  | Conll arr -> current_position := 0; update_source ()
  | _ -> Log.critical "[first] can be use only with CONLL data"

(* -------------------------------------------------------------------------------- *)
let last () =
  match !current_data with
  | Conll arr -> current_position :=((Array.length arr) - 1); update_source ()
  | _ -> Log.critical "[last] can be use only with CONLL data"

(* -------------------------------------------------------------------------------- *)
let next () = 
  match (!current_data, !current_position) with
    | (Conll arr, p)  when p < (Array.length arr) - 1 ->
      current_position := p+1; update_source ()
  | _ -> Log.critical "[next] can be use only with CONLL data"

(* -------------------------------------------------------------------------------- *)
let has_next () =
  match (!current_data, !current_position) with
    | (Conll arr, p) -> p < (Array.length arr)-1
    | _ -> false

(* -------------------------------------------------------------------------------- *)
let prev () = 
  match (!current_data, !current_position) with
    | (Conll _, p)  when p > 0 ->
      current_position := p-1; update_source ()
  | _ -> Log.critical "[prev] can be use only with CONLL data"

(* -------------------------------------------------------------------------------- *)
let has_prev () = 
  match (!current_data, !current_position) with
    | (Conll _, p) -> p > 0
    | _ -> false

(* -------------------------------------------------------------------------------- *)
let view_label () =
  match (!current_data, !current_position) with
    | (Conll [|(id,_)|], 0) when id="00001" -> "no_id"
    | (Conll [|(id,_)|], 0) -> sprintf "[%s]" id
    | (Conll a, p) ->
      let (id, _) = a.(p) in
      if id = sprintf "%05d" p
      then sprintf "(%d/%d) [no_id]" (p+1) (Array.length a)
      else sprintf "(%d/%d) [%s]" (p+1) (Array.length a) id
    | _ -> "no_id"

(* -------------------------------------------------------------------------------- *)
let file_label () =
  match !input_file with
    | None -> "No file loaded"
    | Some in_file -> in_file


(* -------------------------------------------------------------------------------- *)
let save file =
  match (!current_data, !current_position) with
  | (Conll arr, index) ->
    arr.(index) <- (fst (arr.(index)), !current_source);
    let out_ch = open_out file in
    Array.iter (fun (_,src) -> fprintf out_ch "%s\n" src) arr;
    close_out out_ch;
    modified := false
  | _ -> Log.critical "[Global.save] can be use only with CONLL data"

(* -------------------------------------------------------------------------------- *)
let conll_array file =
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
    done; assert false
  with End_of_file ->
    (match !sentid with
      | Some si -> current_list := (si, Buffer.contents buff) :: !current_list
      | None -> ());
    Array.of_list (List.rev !current_list)

(* -------------------------------------------------------------------------------- *)
let load file =
  try
    match Format.get file with
    | Format.Dep -> let dep = File.read file in current_data := Dep (Dep2pict.from_dep dep, dep)
    | Format.Conll -> current_data := Conll (conll_array file)
    | _ ->
      Log.fwarning "No valid input format detected for file \"%s\", try to guess...\n%!" file;
      let text = File.read file in
      if String.length text > 0 && text.[0] = '1'
      then current_data := Conll (conll_array file)
      else current_data := Dep (Dep2pict.from_dep text, text)
  with
  | Dep2pict.Parse_error msgs -> List.iter (fun (l,m) -> printf "Line %d: %s\n" l m) msgs; critical "Parse error !!"
  | Dep2pict.Id_already_in_use_ id -> critical "Id already in use : %s" id
  | Dep2pict.Unknown_index id -> critical "Can't find index: %s" id
  | Dep2pict.Loop_in_dep msg -> critical "Loop in dependency : %s" msg
