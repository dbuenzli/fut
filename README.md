Fut — Future values for asynchronous programming in OCaml
-------------------------------------------------------------------------------
Release %%VERSION%%

Fut is an OCaml module for asynchronous programming in OCaml. It
provides support to program with future values. A future value is a
value becomes available at some point in the future.

Fut is distributed under the BSD3 license. 

Home page: http://erratique.ch/software/fut    
Contact: Daniel Bünzli `<daniel.buenzl i@erratique.ch>`


## Installation

Fut can be installed with `opam`:

    opam install fut
    opam install js_of_ocaml fut

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.


## Documentation

The documentation and API reference is automatically generated by
`ocamldoc` from the interfaces. It can be consulted [online][3]
and there is a generated version in the `doc` directory of the 
distribution. 

[3]: http://erratique.ch/software/fut/doc/Fut


## Sample programs

If you installed Cmdliner with `opam` sample programs are located in
the directory `opam config var fut:doc`. 

In the distribution sample programs are located in the `test`
directory of the distribution. They can be built with:

    ocamlbuild -use-ocamlfind test/tests.otarget

The resulting binaries are in `_build/test`.

- `test.native` tests the library, nothing should fail.
