module M = Map.Make(String)
type t = string M.t

let write (t : t) path offset data = 
  let v = match M.find_opt path t with
   | None  -> String.make offset ' ' ^ data 
   | Some value -> 
       let lv = String.length value
       and ld = String.length data in
       let end_prefix, begin_suffix = min offset lv, min (offset + ld) lv in
       let prefix, suffix = String.sub value 0 end_prefix, String.sub value begin_suffix (lv - begin_suffix) 
       and padding = String.make (max 0 (offset - lv)) ' ' in 
       prefix ^ padding ^ data ^ suffix
  in
  M.add path v t

let pp fmt t =
  Fmt.(list ~sep:(unit ";") (pair ~sep:(unit ",") string string))
  fmt @@ M.bindings t

let equal t t' = M.equal String.equal t t'
