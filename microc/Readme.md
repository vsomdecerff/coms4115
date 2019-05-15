### Build the ASP compiler

```
ocamlbuild -pkgs llvm asp.native
```

### Run the ASP compiler and generate llvm code
```
./asp.native -l example.asp > example.out
```

### Run the llvm code
```
lli example.out
```

### Compiler files
-  `ast.ml`: abstract syntax tree (AST) definition
-  `scanner.mll`: scanner
-  `aspcparse.mly`: parser
-  `sast.ml`: definition of the semantically-checked AST
-  `semant.ml`: semantic checking
-  `irgen.ml`: LLVM IR code generator

### Other files

- `asp.ml`: top-level file to test and run asp compiler
- `example.asp`: a sample asp source code
- `example.out`: a sample compiled code of example.asp
