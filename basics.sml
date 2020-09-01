(*
SML (Standard Meta Language)

1. SML/NJ - New Jersey

- Interpreter
- Is quick to use
- Is good for interactive sessions
- Standalone programs can be written but not recommended

2. Mlton

- Full program optimising compiler
- Slow to use
- Produces standalone fast executables


Bindings

1. A program is a list of bindings.
2. Each binding defines a variablea and associates it with a value.
3. The value associated to a variable is obtained by reducing/simplifying the RHS.
4. Computations happen while these reductions are done.

Pure values vs evaluations with side effects


* In general it is always preferable to use pure values and use side-effects only when necessary because the value does reflect what happens.

*)


