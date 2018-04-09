module Map = Mirage_fs_mem.Pure.M
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
let map = Map.add "a" (false, bc) Map.empty

let empty () =
  let expected = Map.empty in
  Alcotest.check compare_t "hello" expected (Mirage_fs_mem.Pure.empty ()) 

let read () =
  let expected = Ok [ bc ] in
  Alcotest.check compare_read_res "hello" expected (Mirage_fs_mem.Pure.read map "a" 0 2) 

let size () =
  let expected = 2L in 
  Alcotest.check Alcotest.int64 "hello" expected (Mirage_fs_mem.Pure.size map "a")

let create () =
  let expected = Ok (Map.add "leer" (false, Cstruct.empty) Map.empty) in
  Alcotest.check compare_write_res "hello" expected (Mirage_fs_mem.Pure.create Map.empty "leer")

let mkdir () =
  let expected = Ok (Map.add "leer" (true, Cstruct.empty) Map.empty) in
  Alcotest.check compare_write_res "hello" expected (Mirage_fs_mem.Pure.mkdir Map.empty "leer")

let destroy () =
  let expected = Map.empty in
  Alcotest.check compare_t "hello" expected (Mirage_fs_mem.Pure.destroy map "a")

let stat () =
  let expected = Ok { Mirage_fs.filename = "a" ; read_only = false ; directory = false ; size = 2L } in
  Alcotest.check compare_stat_res "hello" expected (Mirage_fs_mem.Pure.stat map "a")
  
let listdir () =
  let map_of_three = Map.add "b" (true, Cstruct.empty) (Map.add "c" (false, Cstruct.empty) map) in
  let expected = [ "a" ; "b" ; "c" ] in 
  Alcotest.check Alcotest.(slist string String.compare) "hello" expected (Mirage_fs_mem.Pure.listdir map_of_three "")

let write () =
  let expected = Ok (Map.add "a" (false, bc) Map.empty) in
  Alcotest.check compare_write_res "hello" expected (Mirage_fs_mem.Pure.write Map.empty "a" 0 bc)

(* value in map is shorter than offset *)
let writeBigOffset () =
  let expected = Ok (Map.add "a" (false, Cstruct.of_string "bc        NEU") Map.empty)
  in
  Alcotest.check compare_write_res "hello" expected (Mirage_fs_mem.Pure.write map "a" 10 neu)

(* value in map is longer than offset*)
let writeSmallOffset () =
  let map = Map.add "a" (false, Cstruct.of_string "gutentag") Map.empty 
  and expected = Ok (Map.add "a" (false, Cstruct.of_string "gNEUntag") Map.empty)
  in
  Alcotest.check compare_write_res "hello" expected (Mirage_fs_mem.Pure.write map "a" 1 neu)

let write_tests = [
  "create empty filesystem", `Quick, empty;
  "reading a file", `Quick, read;
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
