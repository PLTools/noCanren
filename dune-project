(lang dune 3.2)

(generate_opam_files true)

(cram enable)

;(license LGPL-3.0-only)

(name noCanren)

(authors
 "https://github.com/Lozov-Petr"
 "https://github.com/Kakadu"
 "https://github.com/dboulytchev")

(maintainers "lozov.peter@gmail.com")

(bug_reports "https://github.com/Lozov-Petr/noCanren/issues")

(homepage "https://github.com/Lozov-Petr/noCanren")

(package
 (name noCanren)
 (synopsis "Translator from subset of OCaml to OCanren")
 (version 0.3.0)
 (depends
  dune
  (ocaml
   (>= "4.13"))
  ocamlfind
  GT
  (OCanren
   (>= "0.3.0~"))
  (OCanren-ppx
   (>= "0.3.0~"))
  (odoc :with-doc)
  (ocamlformat
   (and
    (>= "0.23")
    :with-test))
  ppx_optcomp
  ppxlib
  ;
  ))
