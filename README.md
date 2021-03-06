Fut — Future values for asynchronous programming in OCaml
-------------------------------------------------------------------------------
Release %%VERSION%%

Fut is a library for asynchronous programming in OCaml. It provides
support to program with values that determine at some point in the
future: future values.

Fut can be used with different schedulers. An API allows to implement
new schedulers. The library comes with a scheduler based on select(2)
that depends on the Unix library and a JavaScript backend that depends
on [js_of_ocaml][1].

Fut and its schedulers are distributed under the ISC license.

Home page: http://erratique.ch/software/fut    
Contact: Daniel Bünzli `<daniel.buenzl i@erratique.ch>`

[1]: http://ocsigen.org/js_of_ocaml/


## Installation

Fut can be installed with `opam`:

    opam install base-unix fut     # select(2) backend and Futu
    opam install js_of_ocaml fut   # js_of_ocaml backend 

If you don't use `opam` consult the [`opam`](opam) file for build
instructions.


## Documentation

The documentation and API reference is automatically generated by
`ocamldoc` from the interfaces. It can be consulted [online][3]
and there is a generated version in the `doc` directory of the 
distribution. 

[3]: http://erratique.ch/software/fut/doc/Fut


## Sample programs

If you installed Fut with `opam` sample programs are located in
the directory `opam config var fut:doc`. 

In the distribution sample programs are located in the `test`
directory of the distribution. They can be built with:

    ocamlbuild -use-ocamlfind test/tests.otarget

The resulting binaries are in `_build/test`.

- `test.native` tests the library, nothing should fail.
