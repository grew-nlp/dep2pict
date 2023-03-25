open Printf
open Dep2pictlib
open Conll

module Log = struct
  let warning_ message =
    ANSITerminal.eprintf [ANSITerminal.blue] "WARNING: %s\n" message;
    Printf.eprintf "%!" (* force synchronous printing *)
    
  let warning message = Printf.ksprintf warning_ message

  let fail_ message =
    ANSITerminal.eprintf [ANSITerminal.red] "FAIL: %s\n" message;
    Printf.eprintf "%!" (* force synchronous printing *);
    exit 1
    
  let fail message = Printf.ksprintf fail_ message
end


exception Found of int

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


exception Error of Yojson.Basic.t

let error ?file ?line ?fct ?data msg =
  let opt_list = [
    Some ("message", `String msg);
    (CCOption.map (fun x -> ("file", `String x)) file);
    (CCOption.map (fun x -> ("line", `Int x)) line);
    Some ("program", `String "dep2pict");
    (CCOption.map (fun x -> ("function", `String x)) fct);
    (CCOption.map (fun x -> ("data", `String x)) data);
  ] in
  let json = `Assoc (CCList.filter_map (fun x->x) opt_list) in
  raise (Error json)


let batch = ref false
let rtl = ref false
let no_root = ref false

let critical msg = ksprintf
  (fun m -> match !batch  with
    | true -> eprintf "%s\n" m; exit 1
    | false -> Log.fail "%s" m
  ) msg

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
  type format =  Dep | Conll | Png | Svg | Pdf | Json | No_suff | Unk of string

  let get file =
    match get_suffix file with
    | None -> Log.warning "Cannot guess format (no suffix) for file \"%s\"" file; No_suff
    | Some suff ->
      try List.assoc suff [ (".dep",Dep); (".conll",Conll); (".conllu",Conll); (".png",Png); (".svg",Svg); (".pdf",Pdf); (".json",Json)]
      with Not_found -> Log.warning "Unkwnow file extension \"%s\" for file \"%s\"" suff file; Unk suff

  let to_string = function
    | Dep -> "dep"
    | Conll -> "conll"
    | Png -> "png"
    | Svg -> "svg"
    | Pdf -> "pdf"
    | Json -> "json"
    | No_suff -> "no_suff"
    | Unk suff -> sprintf "unknown suffix '%s'" suff
end

let dir = DATA_DIR

let (input_file : string option ref) = ref None
let (input_last_modifaction_time) = ref 0.

let (output_file : string option ref) = ref None

type input_data =
  | Dep of Dep2pictlib.t
  | Conll of (string * Conll.t) array

let current_data = ref (Conll [||])
let (current_position : int ref) = ref 0   (* position of the current_focus in the current_array *)

let debug = ref false

let requested_sentid = ref None

(* the name of the file containing special chars (like korean chars) *)
let (special_chars : string option ref) = ref None

let get_id () =
  match !current_data with
  | Conll arr -> fst (arr.(!current_position))
  | _ -> Log.fail "[get_id] can be use only with CONLL data"

(* -------------------------------------------------------------------------------- *)
let array_assoc key array =
  try
    Array.iteri (fun i (k,_) -> if k = key then raise (Found i)) array;
    None
  with Found i -> Some i

(* -------------------------------------------------------------------------------- *)
let search_sentid sentid =
  match !current_data with
  | Conll arr ->
    begin
      match array_assoc sentid arr with
      | Some p -> current_position := p
      | None -> Log.fail "No conll struct with name \"%s\"" sentid
    end
  | _ -> Log.fail "[search_sentid] can be use only with CONLL data"

(* -------------------------------------------------------------------------------- *)
let set_position () =
  match (!current_data, !current_position, !requested_sentid) with
  | (Conll _, _, Some sentid) -> search_sentid sentid
  | (Conll arr, p, None) when p < 0 || p >= (Array.length arr) ->
    Log.warning "position %d is out of bounds, set position to 0" p;
    current_position := 0
  | (Conll _, p, None) -> current_position := p
  | (_, _, Some _) -> Log.fail "Options --sentid can be used only with CONLL input"
  | (_, _, None) -> ()

(* -------------------------------------------------------------------------------- *)
let first () =
  match !current_data with
  | Conll arr -> current_position := 0
  | _ -> Log.fail "[first] can be use only with CONLL data"

(* -------------------------------------------------------------------------------- *)
let last () =
  match !current_data with
  | Conll arr -> current_position :=((Array.length arr) - 1)
  | _ -> Log.fail "[last] can be use only with CONLL data"

(* -------------------------------------------------------------------------------- *)
let next () =
  match (!current_data, !current_position) with
    | (Conll arr, p)  when p < (Array.length arr) - 1 ->
      current_position := p+1
  | _ -> Log.fail "[next] can be use only with CONLL data"

(* -------------------------------------------------------------------------------- *)
let has_next () =
  match (!current_data, !current_position) with
    | (Conll arr, p) -> p < (Array.length arr)-1
    | _ -> false

(* -------------------------------------------------------------------------------- *)
let prev () =
  match (!current_data, !current_position) with
    | (Conll _, p)  when p > 0 ->
      current_position := p-1
  | _ -> Log.fail "[prev] can be use only with CONLL data"

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
let load file =
  match Format.get file with
  | Format.Dep -> let dep = File.read file in current_data := Dep (Dep2pictlib.from_dep dep)
  | Format.Conll -> current_data := Conll (Conll_corpus.get_data (Conll_corpus.load ~config:(Conll_config.build "ud") file))
  | _ ->
    Log.warning "No valid input format detected for file \"%s\", try to guess...\n%!" file;
    let text = File.read file in
    if String.length text > 0 && (text.[0] = '1' || text.[0] = '#')
    then current_data := Conll (Conll_corpus.get_data (Conll_corpus.load file))
    else current_data := Dep (Dep2pictlib.from_dep text)
