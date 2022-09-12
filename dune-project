(lang dune 3.4)

(name simple_sequence)

(generate_opam_files true)

(source
 (github QAhell/simple_sequence_ocaml))

(authors "Armin Heller")

(maintainers "Armin Heller")

(license Apache-2.0)

(documentation https://github.com/QAhell/simple_sequence_ocaml)

(package
 (name simple_sequence)
 (version 0.1)
 (synopsis "An interface for sequences and implementations based on lists, lazy lists and arrays")
 (description "An interface for sequences and implementations based on lists, lazy lists and arrays")
 (depends ocaml dune)
 (tags
    (topics sequence)))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
