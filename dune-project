(lang dune 3.4)

(name sequence)

(generate_opam_files true)

(source
 (github QAhell/sequence_ocaml))

(authors "Armin Heller")

(maintainers "Armin Heller")

(license Apache-2.0)

(documentation https://github.com/QAhell/sequence_ocaml)

(package
 (name sequence)
 (version 0.1)
 (synopsis "An interface for sequences and implementations based on lists, lazy lists and arrays")
 (description "An interface for sequences and implementations based on lists, lazy lists and arrays")
 (depends ocaml dune)
 (tags
    (topics sequence)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
