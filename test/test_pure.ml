
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

let key_test =
  let module M = struct
    type t = Mirage_kv.Key.t
    let pp = Mirage_kv.Key.pp
    let equal = Mirage_kv.Key.equal
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

let int63_test =
  let module M = struct
    type t = Optint.Int63.t
    let pp = Optint.Int63.pp
    let equal = Optint.Int63.equal
  end in
  (module M: Alcotest.TESTABLE with type t = M.t)

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
  Alcotest.check compare_read_res "hello" (Ok "bc")
    (Pure.get_partial map key_a ~offset:(Optint.Int63.of_int 0) ~length:2);
  Alcotest.check compare_read_res "hello" (Ok "c")
    (Pure.get_partial map key_a ~offset:(Optint.Int63.of_int 1) ~length:1);
  Alcotest.check compare_read_res "hello" (Ok "b")
    (Pure.get_partial map key_a ~offset:(Optint.Int63.of_int 0) ~length:1);
  Alcotest.check compare_read_res "hello" (Ok "")
    (Pure.get_partial map key_a ~offset:(Optint.Int63.of_int 3) ~length:1);
  Alcotest.check compare_read_res "hello" (Ok "c")
    (Pure.get_partial map key_a ~offset:(Optint.Int63.of_int 1) ~length:4)

let destroy () =
  let expected = empty_m in
  Alcotest.check compare_write_res "hello" (Ok expected)
    (Pure.remove map key_a now)

type node = [ `Value | `Dictionary ]

let pp_node ppf = function
  | `Value -> Fmt.string ppf "value"
  | `Dictionary -> Fmt.string ppf "dictionary"

let equal_node a b = match a, b with
  | `Value, `Value | `Dictionary, `Dictionary -> true
  | _ -> false

let list () =
  let map_of_three = add (key_of_str "b") "" (add (key_of_str "c") "" map) in
  let expected = Ok [ (key_of_str "a", `Value) ; (key_of_str "b", `Value) ; (key_of_str "c", `Value) ] in
  Alcotest.check
    Alcotest.(result (slist (pair key_test (testable pp_node equal_node)) compare) e)
    "hello" expected (Pure.list map_of_three Mirage_kv.Key.empty)

let write () =
  let expected = Ok (add key_a bc empty_m) in
  Alcotest.check compare_write_res "hello" expected
    (Pure.set empty_m key_a now bc)

let write_partial () =
  let expected = Ok (add key_a bc empty_m) in
  Alcotest.check compare_write_res __LOC__ expected
    (Pure.set_partial empty_m key_a now ~offset:(Optint.Int63.of_int 1) bc);
  Alcotest.check compare_write_res __LOC__ expected
    (Pure.set_partial empty_m key_a now ~offset:(Optint.Int63.of_int 2) bc);
  match Pure.set empty_m key_a now bc with
  | Error _ -> Alcotest.fail "unexpected set result"
  | Ok m ->
    let exp = Ok (add key_a "bbc" empty_m) in
    Alcotest.check compare_write_res __LOC__ exp
      (Pure.set_partial m key_a now ~offset:(Optint.Int63.of_int 1) bc);
    let exp = Ok (add key_a "bcbc" empty_m) in
    Alcotest.check compare_write_res __LOC__ exp
      (Pure.set_partial m key_a now ~offset:(Optint.Int63.of_int 2) bc);
    Alcotest.check compare_write_res __LOC__ exp
      (Pure.set_partial m key_a now ~offset:(Optint.Int63.of_int 10) bc)

let write_multiple () =
  let expected = Ok (add key_a bc (add (key_of_str "b") bc empty_m)) in
  match Pure.set empty_m (key_of_str "b") now bc with
  | Ok m -> Alcotest.check compare_write_res "hello" expected
              (Pure.set m key_a now bc)
  | Error _ -> Alcotest.fail "Unexpected map write result"

let size () =
  Alcotest.(check (result int63_test e) __LOC__ (Ok (Optint.Int63.of_int 2))
              (Pure.size map key_a))

let rename () =
  let expected = Ok (add key_a bc empty_m) in
  match Pure.set empty_m (key_of_str "b") now bc with
  | Ok m -> Alcotest.check compare_write_res "hello" expected
              (Pure.rename m ~source:(key_of_str "b") ~dest:key_a now)
  | Error _ -> Alcotest.fail "Unexpected map write result"

let rename_replace () =
  let expected = Ok (add key_a bc empty_m) in
  match Pure.set empty_m (key_of_str "b") now bc with
  | Error _ -> Alcotest.fail "Unexpected map write result"
  | Ok m ->
    match Pure.set m (key_of_str "a") now neu with
    | Error _ -> Alcotest.fail "Unexpected map write result"
    | Ok m ->
      Alcotest.check compare_write_res "hello" expected
        (Pure.rename m ~source:(key_of_str "b") ~dest:key_a now)

let rename_value_to_dict () =
  let expected =
    Ok (add (key_of_str "a/b") bc
          (add (key_of_str "a/a") neu empty_m))
  in
  match Pure.set empty_m (key_of_str "b") now bc with
  | Error _ -> Alcotest.fail "Unexpected map write result"
  | Ok m ->
    match Pure.set m (key_of_str "a/a") now neu with
    | Ok m ->
      Alcotest.check compare_write_res "hello" expected
        (Pure.rename m ~source:(key_of_str "b") ~dest:key_a now)
    | Error _ -> Alcotest.fail "Unexpected map write result"

let rename_dict () =
  let expected =
    Ok (add (key_of_str "b/b") neu
          (add (key_of_str "b/a") bc empty_m))
  in
  match Pure.set empty_m (key_of_str "a/a") now bc with
  | Error _ -> Alcotest.fail "Unexpected map write result"
  | Ok m ->
    match Pure.set m (key_of_str "a/b") now neu with
    | Ok m ->
      Alcotest.check compare_write_res "hello" expected
        (Pure.rename m ~source:(key_of_str "a") ~dest:(key_of_str "b") now)
    | Error _ -> Alcotest.fail "Unexpected map write result"

let rename_dict_to_value () =
  let expected = Error (`Value_expected (key_of_str "a")) in
  match Pure.set empty_m (key_of_str "a/a") now bc with
  | Error _ -> Alcotest.fail "Unexpected map write result"
  | Ok m ->
    match Pure.set m (key_of_str "b") now neu with
    | Ok m ->
      Alcotest.check compare_write_res "hello" expected
        (Pure.rename m ~source:(key_of_str "a") ~dest:(key_of_str "b") now)
    | Error _ -> Alcotest.fail "Unexpected map write result"

let rename_dict_to_dict () =
  let expected =
    Ok (add (key_of_str "b/b") neu
          (add (key_of_str "b/a/a") bc empty_m))
  in
  match Pure.set empty_m (key_of_str "a/a") now bc with
  | Error _ -> Alcotest.fail "Unexpected map write result"
  | Ok m ->
    match Pure.set m (key_of_str "b/b") now neu with
    | Ok m ->
      Alcotest.check compare_write_res "hello" expected
        (Pure.rename m ~source:(key_of_str "a") ~dest:(key_of_str "b") now)
    | Error _ -> Alcotest.fail "Unexpected map write result"

let rename_dict_to_subdir () =
  let expected = Error (`Rename_source_prefix (key_of_str "a", key_of_str "a/b")) in
  match Pure.set empty_m (key_of_str "a/a") now bc with
  | Error _ -> Alcotest.fail "Unexpected map write result"
  | Ok m ->
    match Pure.set m (key_of_str "a/b/b") now bc with
    | Error _ -> Alcotest.fail "Unexpected map write result"
    | Ok m ->
      Alcotest.check compare_write_res "hello" expected
        (Pure.rename m ~source:(key_of_str "a") ~dest:(key_of_str "a/b") now)

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
  "rename replace", `Quick, rename_replace;
  "rename value to dict", `Quick, rename_value_to_dict;
  "rename dict", `Quick, rename_dict;
  "rename dict to value", `Quick, rename_dict_to_value;
  "rename dict to dict", `Quick, rename_dict_to_dict;
  "rename dict to subdir", `Quick, rename_dict_to_subdir;
]

let tests = [
  "tests", tests;
]

let () =
  Printexc.record_backtrace true;
  Alcotest.run "mirage-kv-mem test" tests
