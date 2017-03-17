all:
	@#ocamlfind opt -rectypes -o sort -syntax camlp5o -package ocanren,ocanren.syntax,GT -linkpkg sort.ml
	@#ocamlfind opt -rectypes -o tree -syntax camlp5o -package ocanren,ocanren.syntax,GT,GT.syntax.all -linkpkg tree.ml
	PATH=`pwd`/../_build/bundle/ocanren:$$PATH OCAMLPATH=../_build/bundle ocamlfind opt -rectypes \
			-package ocanren,ocanren.ppx,ocanren.ppx_gt -c quine_decls.ml
	PATH=`pwd`/../_build/bundle/ocanren:$$PATH OCAMLPATH=../_build/bundle ocamlfind opt -rectypes \
			-o thrines -package ocanren -linkpkg quine_decls.cmx thrines.ml

clean:
	$(RM) *.cm[oix] *.o sort tree
