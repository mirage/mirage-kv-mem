type write_error = [ Mirage_kv.write_error | `Directory_not_empty ]

let pp_write_error ppf = function
  | #Mirage_kv.write_error as e -> Mirage_kv.pp_write_error ppf e
  | `Directory_not_empty -> Fmt.string ppf "directory not empty"

module Pure = struct

  open Rresult.R.Infix

  module M = Map.Make(String)

  type t =
   | Directory of t M.t
   | File of string

  type key = Mirage_kv.Key.t

  let empty () = Directory M.empty

  let find_file_or_directory t key =
    let rec find t = function
      | [] -> Ok t
      | hd::tl -> match t with
        | File _xs -> Error (`Dictionary_expected key)
        | Directory m ->
          match M.find_opt hd m with
          | Some t' -> find t' tl
          | None -> Error (`Not_found key)
    in
    find t (Mirage_kv.Key.segments key)

  let add_file_or_directory t key value =
    let rec add t' = function
      | [] -> Ok value
      | [x] ->
        begin match t' with
          | File _ -> Error (`Dictionary_expected key)
          | Directory m -> Ok (Directory (M.add x value m))
        end
      | hd::tl ->
        begin
          match t' with
          | File _ -> Error (`Dictionary_expected key)
          | Directory m ->
            let node = match M.find_opt hd m with
              | None -> Directory M.empty
              | Some t'' -> t''
            in
            add node tl >>= fun t''' ->
            let m' = M.add hd t''' m in
            Ok (Directory m')
        end
    in
    add t (Mirage_kv.Key.segments key)

  let remove_file_or_directory t key =
    let rec remove t = function
      | [] -> Ok (Directory M.empty)
      | [x] -> begin match t with
          | File _ -> Error (`Dictionary_expected key)
          | Directory m ->
            let m' = M.remove x m in
            Ok (Directory m')
        end
      | hd::tl -> match t with
        | File _ -> Error (`Dictionary_expected key)
        | Directory m ->
          (match M.find_opt hd m with
           | None -> Error (`Dictionary_expected key)
           | Some t' -> Ok t') >>= fun node ->
          remove node tl >>| fun t' ->
          let m' = M.add hd t' m in
          Directory m'
    in
    remove t (Mirage_kv.Key.segments key)

  let get (t : t) key =
    find_file_or_directory t key >>= function
    | Directory _ -> Error (`Value_expected key)
    | File value -> Ok value

  let remove (t : t) path = remove_file_or_directory t path

  let list (t : t) key =
    find_file_or_directory t key >>= function
    | File _ -> Error (`Dictionary_expected key)
    | Directory m -> Ok (List.map (fun (key, value) -> key, match value with File _ -> `Value | Directory _ -> `Dictionary) @@ M.bindings m)

  let set (t : t) key data =
    add_file_or_directory t key (File data)

  let pp fmt t =
    let rec pp_things ?(prefix = "") () fmt = function
      | File v -> Fmt.pf fmt "File %s %d: %s@." prefix (String.length v) v
      | Directory m ->
        List.iter (fun (k, v) ->
            pp_things ~prefix:(prefix ^ "/" ^ k) () fmt v)
          (M.bindings m)
    in
    pp_things () fmt t

  let rec equal t t' = match t, t' with
    | File v, File v' -> String.equal v v'
    | Directory m, Directory m' -> M.equal equal m m'
    | _ -> false

end

type error = Mirage_kv.error
let pp_error = Mirage_kv.pp_error

type +'a io = 'a Lwt.t
type value = string
type key = Mirage_kv.Key.t

let last_modified _ _ = assert false
let digest _ _ = assert false
let batch _ ?retries:_ _ = assert false

let connect _s = Lwt.return (ref (Pure.empty ()))
let disconnect _t = Lwt.return ()

type t = Pure.t ref

let exists m key = Lwt.return @@
  match Pure.find_file_or_directory !m key with
  | Ok (File _) -> Ok (Some `Value)
  | Ok (Directory _) -> Ok (Some `Dictionary)
  | Error (`Not_found _) -> Ok None
  | Error e -> Error e

let get m path = Lwt.return @@ Pure.get !m path

let remove m path = Lwt.return @@ match Pure.remove !m path with
  | Error e -> Error e
  | Ok m' -> m := m'; Ok ()

let list m path = Lwt.return @@ Pure.list !m path

let set m path data = Lwt.return @@ match Pure.set !m path data with
  | Error e -> Error e
  | Ok m' -> m := m'; Ok ()

let pp fmt m = Pure.pp fmt !m

let equal a b = Pure.equal !a !b
