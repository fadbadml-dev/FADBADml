(lang dune 2.7)
(name fadbadml)
(version 0.1.2.2)

(generate_opam_files true)

(source (github fadbadml-dev/FADBADml))
(homepage "https://fadbadml-dev.github.io/FADBADml/")
(documentation https://fadbadml-dev.github.io/FADBADml/doc/)
(license CECILL-C)
(authors "François Bidet" "Ismail Bennani")
(maintainers "fbidet@lix.polytechnique.fr" "ismail.lahkim.bennani@ens.fr")

(package 
    (name fadbadml)
    (synopsis "FADBAD++ for OCaml")
    (description "This library is an OCaml porting of FADBAD++, a software written by Ole Stauning and Claus Bendtsen.")
    (depends (ocaml (>= "4.08")) (qcheck :with-test) (conf-python-3 :with-test)))