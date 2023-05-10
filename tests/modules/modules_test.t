  $ cat > modules.ml <<-EOF
  > module type A = sig
  >   val f : bool -> bool
  > end
  > module type B = sig
  >   type t = bool
  >   val f : bool -> bool
  > end
  > [@@same_in_ocanren]
  $ cat modules.ml
  module type A = sig
    val f : bool -> bool
  end
  module type B = sig
    type t = bool
    val f : bool -> bool
  end
  [@@same_in_ocanren]
  $ ocamlfind c modules.ml -linkpkg -o modules.out
  $ rm -fr modules.out
  $ noCanren modules.ml -high-order-mode -o modules.out
  $ cat modules.out
  [@@@ocaml.warning "-8"]
  module type A  = sig val f : bool -> bool end
  module type B  = sig type t = bool val f : bool -> bool end[@@same_in_ocanren ]
  [@@@ocaml.warning "+8"]
  open GT
  open OCanren
  module HO =
    struct
      module type A  = sig module HO : sig val f : (bool OCanren.ilogic -> OCanren.goal) -> bool OCanren.ilogic -> OCanren.goal end end
      module type B  = sig type nonrec t = bool val f : bool -> bool end
    end
  module FO = struct open HO end
