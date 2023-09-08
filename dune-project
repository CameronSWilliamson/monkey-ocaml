(lang dune 3.10)

(name monkey)

(generate_opam_files true)

(source
 (github cameronswilliamson/monkey-ocaml))

(authors "Cameron S. Williamson")

(maintainers "Cameron S. Williamson")

(license MIT)

(documentation https://url/to/documentation)

(package
 (name monkey)
 (synopsis "Monkey programming language")
 (description "A C-like language named Monkey PL")
 (depends ocaml dune base fmt ppx_inline_test ppx_expect ppx_deriving stdio core))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
