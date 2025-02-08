type write_error = Mirage_kv.write_error
let pp_write_error = Mirage_kv.pp_write_error

type error = Mirage_kv.error
let pp_error = Mirage_kv.pp_error

module Pure = struct

  module M = Map.Make(String)

  let ( let* ) = Result.bind

  type t =
   | Dictionary of Ptime.t * t M.t
   | Value of Ptime.t * string

  type key = Mirage_kv.Key.t

  let empty now () = Dictionary (now, M.empty)

  let get_node t key =
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
    let* v = get_node t key in
    match v with
    | Dictionary _ -> Error (`Value_expected key)
    | Value (_, value) -> Ok value

  let size t key =
    let* v = get t key in
    Ok (Optint.Int63.of_int (String.length v))

  let get_partial t key ~offset ~length =
    let* v = get t key in
    if Int64.of_int (String.length v) < Optint.Int63.to_int64 offset then
      Ok ""
    else
      let off = Optint.Int63.to_int offset in
      Ok (String.sub v off (min (String.length v - off) length))

  let last_modified t key =
    let* v = get_node t key in
    match v with
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
          let* node =
            match M.find_opt hd m with
            | None -> Error (`Dictionary_expected key)
            | Some t' -> Ok t'
          in
          let* t' = remove node tl in
          let m' = M.add hd t' m in
          Ok (Dictionary (mtime, m'))
    in
    remove t (Mirage_kv.Key.segments key)

  let list t key =
    let* v = get_node t key in
    match v with
    | Value _ -> Error (`Dictionary_expected key)
    | Dictionary (_, m) ->
      let name_and_kind (k, v) =
        Mirage_kv.Key.add key k,
        match v with Value _ -> `Value | Dictionary _ -> `Dictionary
      in
      Ok (List.map name_and_kind @@ M.bindings m)

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
            let* t''' = add node tl in
            let m' = M.add hd t''' m in
            Ok (Dictionary (mtime, m'))
        end
    in
    add t (Mirage_kv.Key.segments key)

  let set_partial t key now ~offset data =
    match get t key with
    | Ok v ->
      let off = Optint.Int63.to_int offset in
      let v' = String.sub v 0 (min off (String.length v)) in
      let v'' =
        let start = min (String.length v) (off + String.length data) in
        String.sub v start (String.length v - start)
      in
      set t key now (v' ^ data ^ v'')
    | Error (`Not_found _) -> set t key now data
    | Error _ as e -> e

  let rename t ~source ~dest now =
    match get_node t source with
    | Error _ as e -> e
    | Ok (Value (n, v)) ->
      let* t = remove t source now in
      begin match get_node t dest with
        | Error _ -> set t dest n v
        | Ok (Value _) -> set t dest n v
        | Ok (Dictionary _) ->
          let* last_seg = match List.rev (Mirage_kv.Key.segments source) with
            | hd::_ -> Ok hd
            | [] -> Error (`Rename_source_prefix (source, dest))
          in
          set t (Mirage_kv.Key.add dest last_seg) n v
      end
    | Ok (Dictionary _ as d) ->
      let set_dictionary t name =
        let rec go t' = function
          | [] -> Ok d
          | [x] ->
            begin match t' with
              | Value _ -> Error (`Dictionary_expected name)
              | Dictionary (_, m) -> Ok (Dictionary (now, M.add x d m))
            end
          | hd::tl ->
            begin
              match t' with
              | Value _ -> Error (`Dictionary_expected name)
              | Dictionary (mtime, m) ->
                let node = match M.find_opt hd m with
                  | None -> Dictionary (now, M.empty)
                  | Some t'' -> t''
                in
                let* t''' = go node tl in
                let m' = M.add hd t''' m in
                Ok (Dictionary (mtime, m'))
            end
        in
        go t (Mirage_kv.Key.segments name)
      in
      match get_node t dest with
      | Error _ ->
        let* t = remove t source now in
        set_dictionary t dest
      | Ok (Value _) -> Error (`Value_expected source)
      | Ok (Dictionary _) ->
        let srcstr = Mirage_kv.Key.to_string source in
        let dststr = Mirage_kv.Key.to_string dest in
        if String.length dststr >= String.length srcstr &&
           String.(equal srcstr (String.sub dststr 0 (String.length srcstr)))
        then
          Error (`Rename_source_prefix (source, dest))
        else
          let* last_seg =
            match List.rev (Mirage_kv.Key.segments source) with
            | [] -> Error (`Rename_source_prefix (source, dest))
            | last_seg :: _ -> Ok last_seg
          in
          let* t = remove t source now in
          set_dictionary t (Mirage_kv.Key.add dest last_seg)

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

type key = Mirage_kv.Key.t

type t = Pure.t ref * (unit -> Ptime.t)

let connect ?(now = Mirage_ptime.now) () =
  Lwt.return (ref (Pure.empty (now ()) ()), now)

let disconnect _t = Lwt.return ()

let last_modified dict key =
  Lwt.return @@ Pure.last_modified !(fst dict) key

let digest dict key =
  Lwt.return @@ match Pure.get_node !(fst dict) key with
  | Ok (Value (_, data)) -> Ok (Digest.string data)
  | Ok (Dictionary (mtime, dict)) ->
    let data = Fmt.to_to_string Pure.pp (Dictionary (mtime, dict)) in
    Ok (Digest.string data)
  | Error e -> Error e

let exists dict key =
  Lwt.return @@ match Pure.get_node !(fst dict) key with
  | Ok (Value _) -> Ok (Some `Value)
  | Ok (Dictionary _) -> Ok (Some `Dictionary)
  | Error (`Not_found _) -> Ok None
  | Error e -> Error e

let get dict key = Lwt.return @@ Pure.get !(fst dict) key

let get_partial dict key ~offset ~length =
  Lwt.return @@ Pure.get_partial !(fst dict) key ~offset ~length

let size dict key = Lwt.return @@ Pure.size !(fst dict) key

let remove dict key = Lwt.return @@ match Pure.remove !(fst dict) key ((snd dict) ()) with
  | Error e -> Error e
  | Ok dict' -> fst dict := dict'; Ok ()

let list dict key = Lwt.return @@ Pure.list !(fst dict) key

let set dict key data = Lwt.return @@ match Pure.set !(fst dict) key ((snd dict) ()) data with
  | Error e -> Error e
  | Ok dict' -> fst dict := dict'; Ok ()

let set_partial dict key ~offset data =
  Lwt.return @@ match Pure.set_partial !(fst dict) key ((snd dict) ()) ~offset data with
  | Error e -> Error e
  | Ok dict' -> fst dict := dict'; Ok ()

let rename dict ~source ~dest =
  Lwt.return @@ match Pure.rename !(fst dict) ~source ~dest ((snd dict) ()) with
  | Error e -> Error e
  | Ok dict' -> fst dict := dict'; Ok ()

let pp fmt dict = Pure.pp fmt !(fst dict)

let equal a b = Pure.equal !(fst a) !(fst b)

let allocate dict key ?last_modified size =
  let open Lwt.Infix in
  exists dict key >|= function
  | Error _ as e -> e
  | Ok Some _ -> Error (`Already_present key)
  | Ok None ->
    let data = String.make (Optint.Int63.to_int size) '\000' in
    let now = Option.value ~default:((snd dict) ()) last_modified in
    match Pure.set !(fst dict) key now data with
    | Error e -> Error e
    | Ok dict' -> fst dict := dict'; Ok ()
