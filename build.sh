#!/usr/bin/env bash
set -x

#ocamlfind c -c -syntax camlp5o -package GT.syntax,typeutil GT.cma minikanren.ml -verbose 
ocamlfind c -c -rectypes Stream.ml && \
ocamlfind c -c -rectypes -syntax camlp5o -package GT,GT.syntax,typeutil,logger.syntax MiniKanren.ml -verbose 
#    ocamlfind c    -rectypes -o test -syntax camlp5o -package GT,GT.syntax,logger.syntax Stream.cmo MiniKanren.cmo test.ml -linkpkg
ocamlfind c -w -8-31   -rectypes -o test -package GT Stream.cmo MiniKanren.cmo test.ml -linkpkg


