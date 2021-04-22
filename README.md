# polynomial
Code for some examples around polynomial used in an OCaml formation

This code contains three implementations of the polynomials in OCaml. It is intended as a
pedagical exercise to demonstrate OCaml.

The three implementations are :

  * `array_polynomial.ml`: an implementation of polynomial over float type using arrays

  * `list_polynomial.ml`: an implementation of polynomial over float type using list

  * `horner_polynomial.ml`: an implementation of polynomial over ratio type using a
    recursive variant type

It relies on modules for enforcing the type of the offered functions. The various module
types are defined within the interface.mli file. Among others, it includes an abstract
notion of ring. A test of the ring axioms is done in the file ring.ml.

`main.ml` tests the three implementations.

The project is managed by dune, so to compile and execute the main.ml use:
```
dune exec ./main.exe
```
