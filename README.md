Categorical Abstract Machine
============================

Author: John L. Singleton <jls@cs.ucf.edu>

```
Usage: cam [ -lex | -parse | -vm | -run | -compile ] FILE
-lex:    	Lex the source of the program contained in FILE.
-parse:		Parse the source of program contained in FILE (implies -lex).
-vm:		Execute the VM code contained in FILE.
-run:       Lex, Parse, and Execute the code contained in FILE.
-compile:   Compile code contained in FILE. VM code will be written to FILE.o. 
```

Program examples are in `programs`.

Requirements
============

1. Haskell Platform: https://www.haskell.org/platform/

2. Stack: https://docs.haskellstack.org/en/stable/README/

Usage 
=====

To build the project, execute `stack build` in the root directory.

Once the project is built you can run it with the following command: 

`stack exec cam -- ARGS`

For example:

`stack exec cam -- -run programs/lambdaconditional2.cam`

This will run cam with the command option `-run` on the file `programs/lambdaconditional2`.


