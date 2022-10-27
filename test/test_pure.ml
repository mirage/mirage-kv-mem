
module Pure = Mirage_kv_mem.Pure

let compare_t =
  let module M = Pure in (module M: Alcotest.TESTABLE with type t = Pure.t)

let we =
  let module M = struct
    type t = Mirage_kv_mem.write_error
    let pp = Mirage_kv_mem.pp_write_error
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
let add k v m = match Pure.set m k now v with
 | Error _ -> assert false
 | Ok m -> m

let empty_m = Pure.empty now ()

let key_of_str = Mirage_kv.Key.v

let key_a = key_of_str "a"

let map = add key_a bc empty_m

let empty () =
  let expected = empty_m in
  Alcotest.check compare_t "hello" expected (Pure.empty now ())

let read () =
  let expected = Ok bc in
  Alcotest.check compare_read_res "hello" expected (Pure.get map key_a)

let read_partial () =
  Alcotest.check compare_read_res "hello" (Ok "bc") (Pure.get_partial map key_a ~offset:0 ~length:2);
  Alcotest.check compare_read_res "hello" (Ok "c") (Pure.get_partial map key_a ~offset:1 ~length:1);
  Alcotest.check compare_read_res "hello" (Ok "b") (Pure.get_partial map key_a ~offset:0 ~length:1);
  Alcotest.check compare_read_res "hello" (Ok "") (Pure.get_partial map key_a ~offset:3 ~length:1);
  Alcotest.check compare_read_res "hello" (Ok "c") (Pure.get_partial map key_a ~offset:1 ~length:4)

let destroy () =
  let expected = empty_m in
  Alcotest.check compare_write_res "hello" (Ok expected)
    (Pure.remove map key_a now)

type node = [ `Value | `Dictionary ] [@@deriving eq, show]

let list () =
  let map_of_three = add (key_of_str "b") "" (add (key_of_str "c") "" map) in
  let expected = Ok [ ("a", `Value) ; ("b", `Value) ; ("c", `Value) ] in
  Alcotest.check
    Alcotest.(result (slist (pair string (testable pp_node equal_node)) compare) e)
    "hello" expected (Pure.list map_of_three Mirage_kv.Key.empty)

let write () =
  let expected = Ok (add key_a bc empty_m) in
  Alcotest.check compare_write_res "hello" expected
    (Pure.set empty_m key_a now bc)

let write_partial () =
  let expected = Ok (add key_a bc empty_m) in
  Alcotest.check compare_write_res __LOC__ expected
    (Pure.set_partial empty_m key_a now ~offset:1 bc);
  Alcotest.check compare_write_res __LOC__ expected
    (Pure.set_partial empty_m key_a now ~offset:2 bc);
  match Pure.set empty_m key_a now bc with
  | Error _ -> Alcotest.fail "unexpected set result"
  | Ok m ->
    let exp = Ok (add key_a "bbc" empty_m) in
    Alcotest.check compare_write_res __LOC__ exp
      (Pure.set_partial m key_a now ~offset:1 bc);
    let exp = Ok (add key_a "bcbc" empty_m) in
    Alcotest.check compare_write_res __LOC__ exp
      (Pure.set_partial m key_a now ~offset:2 bc);
    Alcotest.check compare_write_res __LOC__ exp
      (Pure.set_partial m key_a now ~offset:10 bc)

let write_multiple () =
  let expected = Ok (add key_a bc (add (key_of_str "b") bc empty_m)) in
  match Pure.set empty_m (key_of_str "b") now bc with
  | Ok m -> Alcotest.check compare_write_res "hello" expected
              (Pure.set m key_a now bc)
  | Error _ -> Alcotest.fail "Unexpected map write result"

let size () =
  Alcotest.(check (result int e) __LOC__ (Ok 2) (Pure.size map key_a))

let rename () =
  let expected = Ok (add key_a bc empty_m) in
  match Pure.set empty_m (key_of_str "b") now bc with
  | Ok m -> Alcotest.check compare_write_res "hello" expected
              (Pure.rename m ~source:(key_of_str "b") ~dest:key_a now)
  | Error _ -> Alcotest.fail "Unexpected map write result"

let tests = [
  "create empty key value store", `Quick, empty;
  "reading a value", `Quick, read;
  "partial reading a value", `Quick, read_partial;
  "remove value", `Quick, destroy;
  "list entries for dictionary", `Quick, list;
  "writing a value", `Quick, write;
  "write partial", `Quick, write_partial;
  "writing multiple values", `Quick, write_multiple;
  "size", `Quick, size;
  "rename", `Quick, rename;
]

let tests = [
  "tests", tests;
]

let () =
  Printexc.record_backtrace true;
  Alcotest.run "mirage-kv-mem test" tests
