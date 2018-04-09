let compare_t = let module M = Mirage_fs_mem.Pure in (module M: Alcotest.TESTABLE with type t = Mirage_fs_mem.Pure.t)

let we =
  let module M = struct
    type t = Mirage_fs.write_error
    let pp = Mirage_fs.pp_write_error
    let equal a b = compare a b = 0
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)
let compare_write_res = Alcotest.result compare_t we

let e =
  let module M = struct
    type t = Mirage_fs.error
    let pp = Mirage_fs.pp_error
    let equal a b = compare a b = 0
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)
let cs =
  let module M = struct
    type t = Cstruct.t
    let pp = Cstruct.hexdump_pp
    let equal = Cstruct.equal
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)
let compare_read_res = Alcotest.result (Alcotest.list cs) e

let stat =
  let open Mirage_fs in
  let module M = struct
    type t = stat
    let pp fmt stat = 
      Fmt.pf fmt "{ name: %s ; read_only: %b ; directory: %b ; size: %Lu }" stat.filename stat.read_only stat.directory stat.size
    let equal a b = 
      String.equal a.filename b.filename && a.read_only = b.read_only && a.directory = b.directory && a.size = b.size 
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)
let compare_stat_res = Alcotest.result stat e

let bc = Cstruct.of_string "bc"
let neu = Cstruct.of_string "NEU"
let add k v m = match Mirage_fs_mem.Pure.write m k 0 v with 
 | Error _ -> assert false
 | Ok m -> m
let empty_m = Mirage_fs_mem.Pure.empty ()

let map = add "a" bc empty_m

let empty () =
  let expected = empty_m in
  Alcotest.check compare_t "hello" expected (Mirage_fs_mem.Pure.empty ()) 

let read () =
  let expected = Ok [ bc ] in
  Alcotest.check compare_read_res "hello" expected (Mirage_fs_mem.Pure.read map "a" 0 2) 

let readWithLength () =
  let expected = Ok [ bc ] in
  Alcotest.check compare_read_res "hello" expected (Mirage_fs_mem.Pure.read map "a" 0 50) 

let size () =
  let expected = Ok 2L in 
  Alcotest.check (Alcotest.result Alcotest.int64 e) "hello" expected (Mirage_fs_mem.Pure.size map "a")

let create () =
  let expected = Ok (add "leer" Cstruct.empty empty_m) in
  Alcotest.check compare_write_res "hello" expected (Mirage_fs_mem.Pure.create empty_m "leer")

let mkdir () =
  let expected = Ok { Mirage_fs.filename = "leer" ; read_only = false ; directory = true ; size = 0L } in
  match Mirage_fs_mem.Pure.mkdir empty_m "leer" with
   | Ok m -> Alcotest.check compare_stat_res "hello" expected (Mirage_fs_mem.Pure.stat m "leer")
   | Error _ -> assert false

let destroy () =
  let expected = empty_m in
  Alcotest.check compare_t "hello" expected (Mirage_fs_mem.Pure.destroy map "a")

let stat () =
  let expected = Ok { Mirage_fs.filename = "a" ; read_only = false ; directory = false ; size = 2L } in
  Alcotest.check compare_stat_res "hello" expected (Mirage_fs_mem.Pure.stat map "a")
  
let listdir () =
  let map_of_three = add "b" Cstruct.empty (add "c" Cstruct.empty map) in
  let expected = [ "a" ; "b" ; "c" ] in 
  Alcotest.check Alcotest.(slist string String.compare) "hello" expected (Mirage_fs_mem.Pure.listdir map_of_three "")

let write () =
  let expected = Ok (add "a" bc empty_m) in
  Alcotest.check compare_write_res "hello" expected (Mirage_fs_mem.Pure.write empty_m "a" 0 bc)

(* value in map is shorter than offset *)
let writeBigOffset () =
  let expected = Ok (add "a" (Cstruct.of_string "bc        NEU") empty_m)
  in
  Alcotest.check compare_write_res "hello" expected (Mirage_fs_mem.Pure.write map "a" 10 neu)

(* value in map is longer than offset*)
let writeSmallOffset () =
  let map = add "a" (Cstruct.of_string "gutentag") empty_m
  and expected = Ok (add "a" (Cstruct.of_string "gNEUntag") empty_m)
  in
  Alcotest.check compare_write_res "hello" expected (Mirage_fs_mem.Pure.write map "a" 1 neu)

let write_tests = [
  "create empty filesystem", `Quick, empty;
  "reading a file", `Quick, read;
  "reading a file giving an illegally long length", `Quick, readWithLength;
  "size of file", `Quick, size;
  "create file", `Quick, create;
  "create directory", `Quick, mkdir;
  "remove file", `Quick, destroy;
  "get stat of file", `Quick, stat;
  "list a directory", `Quick, listdir;
  "writing a file", `Quick, write;
  "writing a file with big offset", `Quick, writeBigOffset;
  "writing a file with small offset", `Quick, writeSmallOffset;
]

let tests = [
  "Write", write_tests;
]

let () =
  Printexc.record_backtrace true;
  Alcotest.run "Mirage-FS-Mem test" tests
