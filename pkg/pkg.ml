#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let () = Pkg.describe "mirage-fs-mem" @@ fun ctx ->
    Ok [
      Pkg.mllib "src/mirage-fs-mem.mllib";
      Pkg.test "test/testPure";
    ]
