(lang dune 3.17)

(name lang)

(generate_opam_files true)

(source
 (github zarakshR/lang))

(authors "Zaraksh Rahman <zarakshR@pm.me>")

(using menhir 3.0)

(package
 (name lang)
 (synopsis "interpreter demonstrating minimal implementation of dynamic types, first-class functions, laziness, mutability, call/cc and shift/reset")
 (depends ocaml ppx_deriving (alcotest :with-test)))
