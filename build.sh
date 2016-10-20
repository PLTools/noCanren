#!/usr/bin/env bash
set -x

#WITH_DEBUG="-ppopt -VIEW -ppopt trace1 -ppopt -LOG"
X="ocamlfind opt -rectypes -g"
#ocamlfind c -c -syntax camlp5o -package GT.syntax,typeutil GT.cma minikanren.ml -verbose
$X -c Stream.ml && \
$X -c -syntax camlp5o -package GT,GT.syntax,typeutil,logger.syntax $WITH_DEBUG MiniKanren.ml -verbose
#    ocamlfind c    -rectypes -o test -syntax camlp5o -package GT,GT.syntax,logger.syntax Stream.cmo MiniKanren.cmo test.ml -linkpkg
$X -w -8-31 -o test -package GT Stream.cmx MiniKanren.cmx test.ml -linkpkg
