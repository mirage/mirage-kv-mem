type write_error = Mirage_kv.write_error

let pp_write_error = Mirage_kv.pp_write_error

module Pure = struct

  open Rresult.R.Infix

  module M = Map.Make(String)

  type t =
   | Dictionary of t M.t
   | Value of string

  type key = Mirage_kv.Key.t

  let empty () = Dictionary M.empty

  let find_file_or_directory t key =
    let rec find t = function
      | [] -> Ok t
      | hd::tl -> match t with
        | Value _xs -> Error (`Dictionary_expected key)
        | Dictionary m ->
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
          | Value _ -> Error (`Dictionary_expected key)
          | Dictionary m -> Ok (Dictionary (M.add x value m))
        end
      | hd::tl ->
        begin
          match t' with
          | Value _ -> Error (`Dictionary_expected key)
          | Dictionary m ->
            let node = match M.find_opt hd m with
              | None -> Dictionary M.empty
              | Some t'' -> t''
            in
            add node tl >>= fun t''' ->
            let m' = M.add hd t''' m in
            Ok (Dictionary m')
        end
    in
    add t (Mirage_kv.Key.segments key)

  let remove_file_or_directory t key =
    let rec remove t = function
      | [] -> Ok (Dictionary M.empty)
      | [x] -> begin match t with
          | Value _ -> Error (`Dictionary_expected key)
          | Dictionary m ->
            let m' = M.remove x m in
            Ok (Dictionary m')
        end
      | hd::tl -> match t with
        | Value _ -> Error (`Dictionary_expected key)
        | Dictionary m ->
          (match M.find_opt hd m with
           | None -> Error (`Dictionary_expected key)
           | Some t' -> Ok t') >>= fun node ->
          remove node tl >>| fun t' ->
          let m' = M.add hd t' m in
          Dictionary m'
    in
    remove t (Mirage_kv.Key.segments key)

  let get (t : t) key =
    find_file_or_directory t key >>= function
    | Dictionary _ -> Error (`Value_expected key)
    | Value value -> Ok value

  let remove (t : t) path = remove_file_or_directory t path

  let list (t : t) key =
    find_file_or_directory t key >>= function
    | Value _ -> Error (`Dictionary_expected key)
    | Dictionary m -> Ok (List.map (fun (key, value) -> key, match value with Value _ -> `Value | Dictionary _ -> `Dictionary) @@ M.bindings m)

  let set (t : t) key data =
    add_file_or_directory t key (Value data)

  let pp fmt t =
    let rec pp_things ?(prefix = "") () fmt = function
      | Value v -> Fmt.pf fmt "Value %s %d: %s@." prefix (String.length v) v
      | Dictionary m ->
        List.iter (fun (k, v) ->
            pp_things ~prefix:(prefix ^ "/" ^ k) () fmt v)
          (M.bindings m)
    in
    pp_things () fmt t

  let rec equal t t' = match t, t' with
    | Value v, Value v' -> String.equal v v'
    | Dictionary m, Dictionary m' -> M.equal equal m m'
    | _ -> false

end

type error = Mirage_kv.error
let pp_error = Mirage_kv.pp_error

type +'a io = 'a Lwt.t
type value = string
type key = Mirage_kv.Key.t

let connect _s = Lwt.return (ref (Pure.empty ()))
let disconnect _t = Lwt.return ()

type t = Pure.t ref

let last_modified _ _ = assert false

let digest m key = Lwt.return @@
  match Pure.get !m key with
  | Ok data -> Ok (Digest.string data)
  | Error (`Value_expected _) ->
    begin match Pure.list !m key with
      | Ok entries ->
        let entry_to_string (name, kind) =
          name ^ match kind with `Value -> "value" | `Dictionary -> "dictionary" in
        Ok (Digest.string (String.concat ";" (List.map entry_to_string entries)))
      | Error e -> Error e
    end
  | Error e -> Error e

let batch m ?retries:_ f = f m
(* //cc samoht is this correct for in-memory store behaviour? *)

let exists m key = Lwt.return @@
  match Pure.find_file_or_directory !m key with
  | Ok (Value _) -> Ok (Some `Value)
  | Ok (Dictionary _) -> Ok (Some `Dictionary)
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
