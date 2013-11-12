The Alpaca Compiler for LLama 
-------------------------

Compiler project for NTUA compilers course. 

The language scpecification can be found [here](http://courses.softlab.ntua.gr/compilers/2012a/llama2012.pdf) in greek.


Llama is a functional programing language, inspired from Caml, featuring:

* Type Inference 
* High order functions
* User defined data types
* Imperative features: destructive arrays, mutable variables, while and for loops


**Dependencies:**

* [*OCaml*](http://ocaml.org/install.html) version 4.0.0 or newer
* [*OCamlgraph*](http://ocamlgraph.lri.fr) or via OPAM [here](http://opam.ocaml.org/pkg/ocamlgraph/1.8.3/)
* [*CamlP5*](http://pauillac.inria.fr/~ddr/camlp5/) or via OPAM [here](http://opam.ocaml.org/pkg/camlp5/6.11/)

**To compile:**

    $ make .depend
    $ make

Alternatively, you can generate a native executable using ocamlopt:

    $ make .depend
    $ make Llama.opt
    
Optionaly, you can create the documentation files: 
    
    $ make doc


**To run:**

    $ ./Llama file.lla
    $ dosbox
    $ ml.exe file.asm llama.lib


You can type 
	
	$ ./llama --help

to see the list of available options.

Have fun!!

[![IMAGE ALT TEXT HERE](http://img.youtube.com/vi/1gZ0x8AVaBI/0.jpg)](http://www.youtube.com/watch?v=1gZ0x8AVaBI)
