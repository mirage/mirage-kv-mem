module Map = Mirage_fs_mem.Pure.M
let m = let module M = Mirage_fs_mem.Pure in (module M: Alcotest.TESTABLE with type t = Mirage_fs_mem.Pure.t)

let bc = Cstruct.of_string "bc"
let neu = Cstruct.of_string "NEU"

let write () =
  let expected = Map.add "a" bc Map.empty in
  Alcotest.check m "hello" expected (Mirage_fs_mem.Pure.write Map.empty "a" 0 bc)

(* value in map is shorter than offset *)
let writeBigOffset () =
  let map = Map.add "a" bc Map.empty 
  and expected = Map.add "a" (Cstruct.of_string "bc        NEU") Map.empty
  in
  Alcotest.check m "hello" expected (Mirage_fs_mem.Pure.write map "a" 10 neu)

(* value in map is longer than offset*)
let writeSmallOffset () =
  let map = Map.add "a" (Cstruct.of_string "gutentag") Map.empty 
  and expected = Map.add "a" (Cstruct.of_string "gNEUntag") Map.empty
  in
  Alcotest.check m "hello" expected (Mirage_fs_mem.Pure.write map "a" 1 neu)

let write_tests = [
  "writing something", `Quick, write;
  "writing with big offset", `Quick, writeBigOffset;
  "writing with small offset", `Quick, writeSmallOffset;
]

let tests = [
  "Write", write_tests;
]

let () =
  Printexc.record_backtrace true;
  Alcotest.run "Mirage-FS-Mem test" tests
