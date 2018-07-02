type write_error = [ Mirage_fs.write_error | `Directory_not_empty ]

let pp_write_error ppf = function
  | #Mirage_fs.write_error as e -> Mirage_fs.pp_write_error ppf e
  | `Directory_not_empty -> Fmt.string ppf "directory not empty"

module Pure = struct

  open Rresult.R.Infix

  module M = Map.Make(String)

  type t =
   | Directory of t M.t
   | File of Cstruct.t

  let empty () = Directory M.empty

  let get_segs p =
    let p' = Astring.String.trim ~drop:(function '/' -> true | _ -> false) p in
    match p' with
    | "" -> []
    | path -> Fpath.(segs (v path))

  let find_file_or_directory t path =
    let segs = get_segs path in
    let rec find t = function
      | [] -> Ok t
      | hd::tl -> match t with
        | File xs -> Error `Not_a_directory
        | Directory m ->
          match M.find_opt hd m with
          | Some t' -> find t' tl
          | None -> Error `No_directory_entry
    in
    find t segs

  let add_file_or_directory t path value =
    let segs = get_segs path in
    let rec add t = function
      | [] -> Ok value
      | hd::tl -> match t with
        | File _ -> Error `Not_a_directory
        | Directory m ->
          let node = match M.find_opt hd m with
            | None -> empty ()
            | Some t' -> t'
          in
          add node tl >>| fun t'' ->
          let m' = M.add hd t'' m in
          Directory m'
    in
    add t segs

  let remove_file_or_directory t path =
    find_file_or_directory t path >>= (function
        | File _ -> Ok ()
        | Directory m when M.is_empty m -> Ok ()
        | _ -> Error `Directory_not_empty) >>= fun () ->
    let segs = get_segs path in
    let rec remove t = function
      | [] -> Error `Not_a_directory
      | [x] -> begin match t with
          | File _ -> Error `Not_a_directory
          | Directory m ->
            let m' = M.remove x m in
            Ok (Directory m')
        end
      | hd::tl -> match t with
        | File _ -> Error `Not_a_directory
        | Directory m ->
          (match M.find_opt hd m with
           | None -> Error `Not_a_directory
           | Some t' -> Ok t') >>= fun node ->
          remove node tl >>| fun t' ->
          let m' = M.add hd t' m in
          Directory m'
    in
    remove t segs

  (* TODO bounds check*)
  let read (t : t) path offset length =
    find_file_or_directory t path >>= function
    | Directory _ -> Error `Is_a_directory
    | File value ->
      let read_length = min length (Cstruct.len value - offset) in
      Ok [Cstruct.sub value offset read_length]

  let size (t : t) path =
    find_file_or_directory t path >>= function
    | Directory _ -> Error `Is_a_directory
    | File value -> Ok (Int64.of_int @@ Cstruct.len value)

  let create t path = add_file_or_directory t path (File Cstruct.empty)
  let mkdir t path = add_file_or_directory t path (empty ())

  let destroy (t : t) path = remove_file_or_directory t path

  let stat (t : t) path =
    find_file_or_directory t path >>= function
    | Directory _ -> Ok Mirage_fs.{ filename = path ; read_only = false ; directory = true ; size = 0L }
    | File value -> Ok Mirage_fs.{ filename = path ; read_only = false ; directory = false ; size = Int64.of_int @@ Cstruct.len value }

  let listdir (t : t) path =
    find_file_or_directory t path >>= function
    | File _ -> Error `Not_a_directory
    | Directory m -> Ok (fst @@ List.split @@ M.bindings m)

  let write (t : t) path offset data =
    match find_file_or_directory t path with
    | Error `No_directory_entry ->
      let buf = Cstruct.create offset in
      Cstruct.memset buf @@ int_of_char ' ';
      let v = Cstruct.append buf data in
      add_file_or_directory t path (File v)
    | Error e -> Error e
    | Ok (Directory _) -> Error `Is_a_directory
    | Ok (File value) ->
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
      add_file_or_directory t path (File v)

  let pp fmt t =
    let rec pp_things ?(prefix = "") () fmt = function
      | File v -> Fmt.pf fmt "File %s %d: %s@." prefix (Cstruct.len v) (Cstruct.to_string v)
      | Directory m ->
        List.iter (fun (k, v) ->
            pp_things ~prefix:(prefix ^ "/" ^ k) () fmt v)
          (M.bindings m)
    in
    pp_things () fmt t

  let rec equal t t' = match t, t' with
    | File v, File v' -> Cstruct.equal v v'
    | Directory m, Directory m' -> M.equal equal m m'
    | _ -> false

end

type error = Mirage_fs.error
let pp_error = Mirage_fs.pp_error 

type +'a io = 'a Lwt.t
type page_aligned_buffer = Cstruct.t

let connect s = Lwt.return (ref (Pure.empty ()))
let disconnect t = Lwt.return ()

type t = Pure.t ref

let read m path offset length = Lwt.return @@ Pure.read !m path offset length

let size m path = Lwt.return @@ Pure.size !m path

let create m path = Lwt.return @@ match Pure.create !m path with
  | Error e -> Error e
  | Ok m' -> m := m'; Ok ()

let mkdir m path = Lwt.return @@ match Pure.mkdir !m path with
  | Error e -> Error e
  | Ok m' -> m := m'; Ok ()

let destroy m path = Lwt.return @@ match Pure.destroy !m path with
  | Error e -> Error e
  | Ok m' -> m := m'; Ok ()

let stat m path = Lwt.return @@ Pure.stat !m path

let listdir m path = Lwt.return @@ Pure.listdir !m path

let write m path offset data = Lwt.return @@ match Pure.write !m path offset data with
  | Error e -> Error e
  | Ok m' -> m := m'; Ok () 

let pp fmt m = Pure.pp fmt !m

let equal a b = Pure.equal !a !b
