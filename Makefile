compile:
	ocamlbuild -use-ocamlfind -classic-display -I src noCanren.native

clean:
	$(RM) -r _build *.native
