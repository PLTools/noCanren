| Upstream master         | Kakadu dune          | Kakadu 4.13          | Kakadu 4.13 + eucpp  |
| ------------------------|----------------------|----------------------|----------------------|
| [![noCanren][1]][2]     | [![noCanren][3]][4]  | [![noCanren][5]][4]  | [![noCanren][6]][4]  |


[1]:  https://github.com/Lozov-Petr/noCanren/workflows/XXXXXXXX/badge.svg?branch=master
[2]:  https://github.com/Lozov-Petr/noCanren/actions
[3]:  https://github.com/Kakadu/noCanren/workflows/Build/badge.svg?branch=dune
[4]:  https://github.com/Kakadu/noCanren/actions
[5]:  https://github.com/Kakadu/noCanren/workflows/Build413/badge.svg?branch=dune-4.13
[6]:  https://github.com/Kakadu/noCanren/workflows/Build413eucpp/badge.svg?branch=dune-4.13-eucpp

# README #

noCanren is translator from subset of OCaml to OCanren.

## Installatiom ##

noCanren can be installing using [opam](https://opam.ocaml.org/doc/Install.html):

`opam pin add noCanren https://github.com/Lozov-Petr/noCanren.git -y`

To run the tests, you should install [OCanren](https://github.com/JetBrains-Research/OCanren) manually.

## Subset of OCaml ##

Available syntax of OCaml subset:

```
S = x, y, z                                   (Variables)
  | fun x -> S                                (Abstaction)
  | S S                                       (Application)
  | false, 1, 2.0, '3', "4", etc.             (Constant)
  | (S, S)                                    (Pair)
  | C (S1, ..., Sn)                           (Constructor)
  | not, (&&), (||), (==), (<>)               (Boolean functions)
  | let f x1 ... xn = S in S                  (Let-binding)
  | let rec f x1 ... xn = S in S              (Recursive let-binding)
  | if S then S else S                        (If-then-else)
  | pattern matching                          (Pattern matching)
```

## Pattern matching ##

Available patterns for pattern matching:
```
P = _                                         (Wildcard)
  | x, y, z                                   (Variables)
  | (P, P)                                    (Pairs)
  | C (P1, ..., Pn)                           (Constructors)
```
Patterns must be disjunct.

Pattern matching can be used in the following form:

```
match S with
| P1 -> S1
| P2 -> S2
...
| Pn -> Sn
```

## Translating ##
Usage:
```
noCanren <options> <input-file>
```

Available options:
```
-o <file>  Set output file name to <file>
-high-order-mode  Switch to high-order mode
-unnesting-mode  Switch to unnesting mode
-without-activate-tactics  Disable activate tactic (only for high-order mode)
-non-deterministic-activate-tactic  Use non-deterministic activate tactic (only for high-order mode)
-deterministic-activate-tactic  Use deterministic activate tactic (only for high-order mode)
-use-call-by-need  Use call-by-need extension of high-order mode (only for high-order mode)
-need-polymorphism  For supporting of polymorphism (only for unnesting mode)
-without-false  For simplified logical relational (only for unnesting mode)
-standart-bool  Enable standart bool relations (only for unnesting mode)
-without-beta-reduction  Disable beta-redactions after conversion (for both modes)
-without-normalization  Disable normalization after conversion (for both modes)
-not-move-unifications  Don't move unifications and disequality constrains after conversion (for both modes)
-leave-constuctors  Conversion is without lawercase-renaming of constructors (for both modes)
-subst-only-util-vars  Use beta-reduction only for additional variables (for both modes)
-spec-tree <file>  Set output file name for specialization tree to <file>
-show-result  Show result of conversion in terminal
-help  Display this list of options
```
