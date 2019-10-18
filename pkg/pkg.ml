#!/usr/bin/env ocaml
#use "topfind"
#require "topkg"
open Topkg

let base_unix = Conf.with_pkg "base-unix"
let react = Conf.with_pkg "react"
let jsoo = Conf.with_pkg "js_of_ocaml"

let jsoo_test ~cond test =
  Pkg.flatten
    [ Pkg.test ~run:false ~cond ~auto:false (test ^ ".js");
      Pkg.test ~run:false ~cond ~auto:false (test ^ ".html"); ]

let () =
  Pkg.describe "fut" @@ fun c ->
  let base_unix = Conf.value c base_unix in
  let react = Conf.value c react in
  let jsoo = Conf.value c jsoo in
  Ok [ Pkg.mllib "src/fut.mllib";
       Pkg.mllib "src/fut_backend_base.mllib";
       Pkg.mllib ~cond:react "src/futr.mllib";
       Pkg.mllib ~cond:base_unix "src/futu.mllib";
       Pkg.mllib ~cond:base_unix "src-select/fut_select.mllib"
         ~dst_dir:"select/";
       Pkg.mllib ~cond:jsoo "src-jsoo/fut_jsoo.mllib" ~dst_dir:"jsoo/";

       Pkg.test "test/test_select";
       jsoo_test ~cond:jsoo "test/test_jsoo";

       Pkg.test "test/cancel";
(*       Pkg.test "test/sbox"; *)
       Pkg.test "test/examples";
(*
       Pkg.test "test/client";
       Pkg.test "test/echoserver";
       Pkg.test "test/uclient";
*)
 ]
