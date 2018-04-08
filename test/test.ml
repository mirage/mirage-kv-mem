module Map = Mirage_fs_mem.M
let m = let module M = Mirage_fs_mem in (module M: Alcotest.TESTABLE with type t = Mirage_fs_mem.t)

let empty () =
  Alcotest.check Alcotest.bool "I am true" true true

let write () =
  let expected = Map.add "a" "bc" Map.empty in
  Alcotest.check m "hello" expected (Mirage_fs_mem.write Map.empty "a" 0 "bc")

(* value in map is shorter than offset *)
let writeBigOffset () =
  let map = Map.add "a" "bc" Map.empty 
  and expected = Map.add "a" "bc        NEU" Map.empty
  in
  Alcotest.check m "hello" expected (Mirage_fs_mem.write map "a" 10 "NEU")

(* value in map is longer than offset*)
let writeSmallOffset () =
  let map = Map.add "a" "gutentag" Map.empty 
  and expected = Map.add "a" "gNEUntag" Map.empty
  in
  Alcotest.check m "hello" expected (Mirage_fs_mem.write map "a" 1 "NEU")

let empty_test = [
  "test the test", `Quick, empty;
  "test writing something", `Quick, write;
  "test writing with big offset", `Quick, writeBigOffset;
  "test writing with small offset", `Quick, writeSmallOffset;
]

let tests = [
  "Empty", empty_test;
]

let () =
  Printexc.record_backtrace true;
  Alcotest.run "Mirage-FS-Mem test" tests
