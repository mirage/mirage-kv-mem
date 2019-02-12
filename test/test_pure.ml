let compare_t = let module M = Mirage_fs_mem.Pure in (module M: Alcotest.TESTABLE with type t = Mirage_fs_mem.Pure.t)

let we =
  let module M = struct
    type t = Mirage_fs_mem.write_error
    let pp = Mirage_fs_mem.pp_write_error
    let equal a b = compare a b = 0
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)
let compare_write_res = Alcotest.result compare_t we

let e =
  let module M = struct
    type t = Mirage_kv.error
    let pp = Mirage_kv.pp_error
    let equal a b = compare a b = 0
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

let compare_read_res = Alcotest.result Alcotest.string e

let now = Ptime.epoch

let bc = "bc"
let neu = "NEU"
let add k v m = match Mirage_fs_mem.Pure.set m k now v with
 | Error _ -> assert false
 | Ok m -> m

let empty_m = Mirage_fs_mem.Pure.empty now ()

let key_a = Mirage_kv.Key.v "a"

let map = add key_a bc empty_m

let empty () =
  let expected = empty_m in
  Alcotest.check compare_t "hello" expected (Mirage_fs_mem.Pure.empty now ())

let read () =
  let expected = Ok bc in
  Alcotest.check compare_read_res "hello" expected (Mirage_fs_mem.Pure.get map key_a)

let destroy () =
  let expected = empty_m in
  Alcotest.check compare_write_res "hello" (Ok expected)
    (Mirage_fs_mem.Pure.remove map key_a now)

type node = [ `Value | `Dictionary ] [@@deriving eq, show]

let list () =
  let map_of_three = add (Mirage_kv.Key.v "b") "" (add (Mirage_kv.Key.v "c") "" map) in
  let expected = Ok [ ("a", `Value) ; ("b", `Value) ; ("c", `Value) ] in
  Alcotest.check
    Alcotest.(result (slist (pair string (testable pp_node equal_node)) compare) e) "hello"
    expected (Mirage_fs_mem.Pure.list map_of_three Mirage_kv.Key.empty)

let write () =
  let expected = Ok (add key_a bc empty_m) in
  Alcotest.check compare_write_res "hello" expected
    (Mirage_fs_mem.Pure.set empty_m key_a now bc)

let write_multiple () =
  let expected = Ok (add key_a bc (add (Mirage_kv.Key.v "b") bc empty_m)) in
  match Mirage_fs_mem.Pure.set empty_m (Mirage_kv.Key.v "b") now bc with
  | Ok m -> Alcotest.check compare_write_res "hello" expected
              (Mirage_fs_mem.Pure.set m key_a now bc)
  | Error _ -> Alcotest.fail "Unexpected map write result"

let write_tests = [
  "create empty filesystem", `Quick, empty;
  "reading a file", `Quick, read;
  "remove file", `Quick, destroy;
  "list a directory", `Quick, list;
  "writing a file", `Quick, write;
  "writing multiple files", `Quick, write_multiple;
]

let tests = [
  "Write", write_tests;
]

let () =
  Printexc.record_backtrace true;
  Alcotest.run "Mirage-FS-Mem test" tests
