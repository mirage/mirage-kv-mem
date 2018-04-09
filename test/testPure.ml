module Map = Mirage_fs_mem.Pure.M
let m = let module M = Mirage_fs_mem.Pure in (module M: Alcotest.TESTABLE with type t = Mirage_fs_mem.Pure.t)

let we = let module M = struct
  type t = Mirage_fs.write_error
  let pp = Mirage_fs.pp_write_error
  let equal a b = compare a b = 0
end 
in (module M: Alcotest.TESTABLE with type t = M.t)
let res = Alcotest.result m we

let bc = Cstruct.of_string "bc"
let neu = Cstruct.of_string "NEU"

let write () =
  let expected = Ok (Map.add "a" (false, bc) Map.empty) in
  Alcotest.check res "hello" expected (Mirage_fs_mem.Pure.write Map.empty "a" 0 bc)

(* value in map is shorter than offset *)
let writeBigOffset () =
  let map = Map.add "a" (false, bc) Map.empty 
  and expected = Ok (Map.add "a" (false, Cstruct.of_string "bc        NEU") Map.empty)
  in
  Alcotest.check res "hello" expected (Mirage_fs_mem.Pure.write map "a" 10 neu)

(* value in map is longer than offset*)
let writeSmallOffset () =
  let map = Map.add "a" (false, Cstruct.of_string "gutentag") Map.empty 
  and expected = Ok (Map.add "a" (false, Cstruct.of_string "gNEUntag") Map.empty)
  in
  Alcotest.check res "hello" expected (Mirage_fs_mem.Pure.write map "a" 1 neu)

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
