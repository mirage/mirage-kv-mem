type write_error = Mirage_kv.write_error
let pp_write_error = Mirage_kv.pp_write_error

type error = Mirage_kv.error
let pp_error = Mirage_kv.pp_error

module Pure = struct

  open Rresult.R.Infix

  module M = Map.Make(String)

  type t =
   | Dictionary of Ptime.t * t M.t
   | Value of Ptime.t * string

  type key = Mirage_kv.Key.t

  let empty now () = Dictionary (now, M.empty)

  let find_file_or_directory t key =
    let rec find t = function
      | [] -> Ok t
      | hd::tl -> match t with
        | Value _ -> Error (`Dictionary_expected key)
        | Dictionary (_, m) ->
          match M.find_opt hd m with
          | Some t' -> find t' tl
          | None -> Error (`Not_found key)
    in
    find t (Mirage_kv.Key.segments key)

  let get t key =
    find_file_or_directory t key >>= function
    | Dictionary _ -> Error (`Value_expected key)
    | Value (_, value) -> Ok value

  let last_modified t key =
    find_file_or_directory t key >>= function
    | Dictionary (mtime, _) -> Ok mtime
    | Value (mtime, _) -> Ok mtime

  let remove t key now =
    let rec remove t = function
      | [] -> Ok (Dictionary (now, M.empty))
      | [x] -> begin match t with
          | Value _ -> Error (`Dictionary_expected key)
          | Dictionary (_, m) ->
            let m' = M.remove x m in
            Ok (Dictionary (now, m'))
        end
      | hd::tl -> match t with
        | Value _ -> Error (`Dictionary_expected key)
        | Dictionary (mtime, m) ->
          (match M.find_opt hd m with
           | None -> Error (`Dictionary_expected key)
           | Some t' -> Ok t') >>= fun node ->
          remove node tl >>| fun t' ->
          let m' = M.add hd t' m in
          Dictionary (mtime, m')
    in
    remove t (Mirage_kv.Key.segments key)

  let list t key =
    find_file_or_directory t key >>= function
    | Value _ -> Error (`Dictionary_expected key)
    | Dictionary (_, m) -> Ok (List.map (fun (key, value) -> key, match value with Value _ -> `Value | Dictionary _ -> `Dictionary) @@ M.bindings m)

  let set t key now data =
    let value = Value (now, data) in
    let rec add t' = function
      | [] -> Ok value
      | [x] ->
        begin match t' with
          | Value _ -> Error (`Dictionary_expected key)
          | Dictionary (_, m) -> Ok (Dictionary (now, M.add x value m))
        end
      | hd::tl ->
        begin
          match t' with
          | Value _ -> Error (`Dictionary_expected key)
          | Dictionary (mtime, m) ->
            let node = match M.find_opt hd m with
              | None -> Dictionary (now, M.empty)
              | Some t'' -> t''
            in
            add node tl >>= fun t''' ->
            let m' = M.add hd t''' m in
            Ok (Dictionary (mtime, m'))
        end
    in
    add t (Mirage_kv.Key.segments key)

  let pp fmt t =
    let rec pp_things ?(prefix = "") () fmt = function
      | Value (mtime, v) -> Fmt.pf fmt "Value %s %d (modified %a): %s@."
                              prefix (String.length v) (Ptime.pp_rfc3339 ()) mtime v
      | Dictionary (_, m) ->
        List.iter (fun (k, v) ->
            pp_things ~prefix:(prefix ^ "/" ^ k) () fmt v)
          (M.bindings m)
    in
    pp_things () fmt t

  let rec equal t t' = match t, t' with
    | Value (_, v), Value (_, v') -> String.equal v v'
    | Dictionary (_, m), Dictionary (_, m') -> M.equal equal m m'
    | _ -> false

end

module Make (CLOCK : Mirage_clock.PCLOCK) = struct
  type +'a io = 'a Lwt.t
  type value = string
  type key = Mirage_kv.Key.t

  [@@@warning "-34"]
  type nonrec error = error
  let pp_error = pp_error

  [@@@warning "-34"]
  type nonrec write_error = write_error
  let pp_write_error = pp_write_error

  let now () = Ptime.v (CLOCK.now_d_ps ())

  let connect _s = Lwt.return (ref (Pure.empty (now ()) ()))
  let disconnect _t = Lwt.return ()

  type t = Pure.t ref

  let last_modified m key =
    Lwt.return @@ match Pure.last_modified !m key with
    | Ok mtime -> Ok Ptime.(Span.to_d_ps (to_span mtime))
    | Error e -> Error e

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

  let remove m path = Lwt.return @@ match Pure.remove !m path (now ()) with
    | Error e -> Error e
    | Ok m' -> m := m'; Ok ()

  let list m path = Lwt.return @@ Pure.list !m path

  let set m path data = Lwt.return @@ match Pure.set !m path (now ()) data with
    | Error e -> Error e
    | Ok m' -> m := m'; Ok ()

  let pp fmt m = Pure.pp fmt !m

  let equal a b = Pure.equal !a !b
end
