(lang dune 2.8)

(generate_opam_files true)

(cram enable)

;(license LGPL-3.0-only)

(authors
 "https://github.com/Lozov-Petr"
 "https://github.com/Kakadu"
 "https://github.com/dboulytchev")

(maintainers "lozov.peter@gmail.com")

(bug_reports "https://github.com/Lozov-Petr/noCanren/issues")

(homepage "https://github.com/Lozov-Petr/noCanren")

(package
 (name noCanren-tests))

(package
 (name noCanren)
 (synopsis "Translator from subset of OCaml to OCanren")
 (version 0.2.0)
 (depends
  dune
  (GT :with-test)
  (OCanren :with-test)
  (odoc :with-doc)
  (ocamlformat :build)
  ppxlib
  ; After adding dependencies to 'dune' files and the same dependecies here too
  ))
