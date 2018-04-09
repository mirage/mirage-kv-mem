type error = Mirage_fs.error
let pp_error = Mirage_fs.pp_error 

type write_error = Mirage_fs.write_error
let pp_write_error = Mirage_fs.pp_write_error 

type +'a io = 'a Lwt.t

let disconnect t = assert false
type page_aligned_buffer = Cstruct.t

module Pure = struct

  module M = Map.Make(String)

  type t = Cstruct.t M.t

  let empty () = M.empty

  let read (t : t) path offset length = assert false
  
  let size (t : t) path = assert false
  
  let create (t : t) path = assert false
  
  let mkdir (t : t) path = assert false
  
  let destroy (t : t) path = assert false
  
  let stat (t : t) path = assert false
  
  let listdir (t : t) path = assert false 
  
  let write (t : t) path offset data = 
    let v = match M.find_opt path t with
     | None  -> let buf = Cstruct.create offset in
                Cstruct.memset buf @@ int_of_char ' ';
                Cstruct.append buf data
     | Some value -> 
         let lv = Cstruct.len value
         and ld = Cstruct.len data in
         let end_prefix, begin_suffix = min offset lv, min (offset + ld) lv in
         let prefix, suffix = Cstruct.sub value 0 end_prefix, Cstruct.sub value begin_suffix (lv - begin_suffix) 
         and padding = 
           let buf = Cstruct.create (max 0 (offset - lv)) in
           Cstruct.memset buf @@ int_of_char ' ';
           buf
         in 
         Cstruct.concat [ prefix ; padding ; data ; suffix ]
    in
    M.add path v t
  
  let pp fmt t =
    Fmt.(list ~sep:(unit ";") (pair ~sep:(unit ",") string Cstruct.hexdump_pp))
    fmt @@ M.bindings t
  
  let equal t t' = M.equal Cstruct.equal t t'

end
