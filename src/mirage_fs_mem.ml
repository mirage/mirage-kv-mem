module Pure = struct

  module M = Map.Make(String)

  type entry = 
   | Directory
   | File of Cstruct.t
  type t = entry M.t

  let empty () = M.empty

  (* TODO bounds check*)
  let read (t : t) path offset length = match M.find_opt path t with
   | None -> Error `No_directory_entry
   | Some Directory -> Error `Is_a_directory
   | Some (File value) -> 
      let read_length = min length (Cstruct.len value - offset) in
      Ok [Cstruct.sub value offset read_length]
  
  let size (t : t) path = match M.find_opt path t with
   | None -> Error `No_directory_entry
   | Some Directory -> Error `Is_a_directory
   | Some (File value) -> Ok (Int64.of_int @@ Cstruct.len value)

  let create_file_or_dir entry (t : t) path = match M.find_opt path t with
   | None -> Ok (M.add path entry t)
   | Some value -> Error `File_already_exists
  let create = create_file_or_dir (File Cstruct.empty) 
  let mkdir = create_file_or_dir Directory
  
  let destroy (t : t) path = M.remove path t
  
  let stat (t : t) path = match M.find_opt path t with
   | None -> Error `No_directory_entry
   | Some Directory -> Ok Mirage_fs.{ filename = path ; read_only = false ; directory = true ; size = 0L }
   | Some (File value) -> Ok Mirage_fs.{ filename = path ; read_only = false ; directory = false ; size = Int64.of_int @@ Cstruct.len value }
  
  let listdir (t : t) path = 
    let pl = String.length path in
    M.fold (fun key value acc -> 
      if String.length key >= pl && String.equal path (String.sub key 0 pl) then
        key :: acc
      else
        acc)
    t []
  
  let write (t : t) path offset data = 
    match M.find_opt path t with
     | None  -> let buf = Cstruct.create offset in
                Cstruct.memset buf @@ int_of_char ' ';
                let v = Cstruct.append buf data in
                Ok (M.add path (File v) t)
     | Some Directory -> Error `Is_a_directory
     | Some (File value) -> 
         let lv = Cstruct.len value
         and ld = Cstruct.len data in
         let end_prefix, begin_suffix = min offset lv, min (offset + ld) lv in
         let prefix, suffix = Cstruct.sub value 0 end_prefix, Cstruct.sub value begin_suffix (lv - begin_suffix) 
         and padding = 
           let buf = Cstruct.create (max 0 (offset - lv)) in
           Cstruct.memset buf @@ int_of_char ' ';
           buf
         in 
         let v = Cstruct.concat [ prefix ; padding ; data ; suffix ] in
         Ok (M.add path (File v) t)
  
  let pp fmt t =
    let pp_entry fmt = function
     | Directory -> Fmt.string fmt "Directory"
     | File v -> Fmt.pf fmt "File %a" Cstruct.hexdump_pp v
    in
    Fmt.(list ~sep:(unit ";") (pair ~sep:(unit ",") string pp_entry))
    fmt @@ M.bindings t
  
  let equal t t' = M.equal (fun a a' -> match a, a' with 
   | Directory, Directory -> true
   | File value, File value' -> Cstruct.equal value value'
   | _ , _ -> false ) t t'

end

type error = Mirage_fs.error
let pp_error = Mirage_fs.pp_error 

type write_error = Mirage_fs.write_error
let pp_write_error = Mirage_fs.pp_write_error 

type +'a io = 'a Lwt.t

let disconnect t = assert false
type page_aligned_buffer = Cstruct.t

type t = Pure.t ref

let read m path offset length = Lwt.return @@ Pure.read !m path offset length

let size m path = Lwt.return @@ Pure.size !m path

let create m path = Lwt.return @@ match Pure.create !m path with
  | Error e -> Error e
  | Ok m' -> m := m'; Ok ()

let mkdir m path = Lwt.return @@ match Pure.mkdir !m path with
  | Error e -> Error e
  | Ok m' -> m := m'; Ok ()

let destroy m path = 
  let m' = Pure.destroy !m path in
  m := m';
  Lwt.return (Ok ())

let stat m path = Lwt.return @@ Pure.stat !m path

let listdir m path = Lwt.return @@ Ok (Pure.listdir !m path)

let write m path offset data = Lwt.return @@ match Pure.write !m path offset data with
  | Error e -> Error e
  | Ok m' -> m := m'; Ok ()

 
