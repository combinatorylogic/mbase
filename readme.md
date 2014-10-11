# COPYING

MBase v 1.0 (c) by Meta Alternative Ltd, 2005-2014

MBase is distributed under the terms of the Q Public License
version 1.0 with a change to choice of law (see license.txt).


# INTRODUCTION

 MBase is a metaprogramming framework built on top of .NET. It is designed
for rapid prototyping and high quality implementation of custom
programming language compilers, but can also serve as a robust and
flexible embeddable scripting engine. Applications are not limited to
.NET platform, since MBase can be used to implement compilers
targeting other platforms - JVM, LLVM, C, etc.

MBase consists of a core language and a library of micro-DSLs.

The core of MBase is built on top of a Lisp-like language. Its 
main applications include:

* DSL compilers and interpreters implementation; 
* Intermediate target language for higher level DSL compilers; 
* Efficient embedded scripting platform.

MBase includes the following micro-DSLs and libraries:

* advanced pattern matching;
* abstract syntax trees support;
* recursive descent parsing;
* Lexing and parsing (LL(*));
* PEG and Pratt parsing;
* embedded Prolog interpreter and compiler;
* extensible syntax frontend;
* list comprehensions;
* XML querying and transforms language;
* embedded .NET IL assembler;
* .NET bindings builder;
* .NET class builder.
* Not.NET low level imperative embedded language;
* Compiler construction modules:
    - Generic register allocation
    - Generic SSA transform and loop analysis
    - Trajan algorithm implementation
* Embedded ML compiler

The core Lisp-like language is not Scheme- or Common Lisp-compliant
since it was designed with very different goals in mind. It is
incrementally built around a tiny core, with reasonable
execution efficiency, and its primary application domain is compilers
development.  Therefore such features as continuations, a
sophisticated type system or floating point arithmetic are not
included in the core language.

Instead MBase provides an inline assembly, a low level 'goto'
statement, a rich set of different pattern matching and AST visiting
features, lexing and parsing aid, various storage data structures
suitable for compilers implementation. The core MBase language should
not normally be used directly, it is rather a target and
implementation tool for higher level languages. There is a rich
extensible syntax frontend for MBase, called PFront, although it is also
not recommended as a "general purpose" programming language.

See an example compiler built on top of MBase and LLVM here:
   https://github.com/combinatorylogic/clike

# BUILDING

MBase requires Mono >= 3.0 and must be installed into GAC.
Graphviz and latex (preferably texlive) are required for building
documentation.
 
To build on Linux with Mono:

```bash
   cd bin
   make
   ./install.sh (may require root rights to access GAC)
```

Optional:
```bash
   ./test.sh
```

Add `$MBASE/bin` to your `$PATH`;

To build on Windows (with Cygwin or mingw32 bash and GNU Make):

```bash
   cd bin
   make MONO= CSC=csc backslash=\\\\
   ./install.sh (as Administrator)
   echo "" > conf
```

Optional
```bash
   ./test.sh
```

# CONTENTS (binaries)

* Core:
    -  MBaseCore.dll, MBaseBin.dll: MBase assemblies
    - repl.exe: simple MBase REPL

* Extra:
    - MBasePackrat.dll: PEG/Pratt compiler
    - MBaseML.dll: Embedded ML compiler
    - MBaseFront.dll: PFront extensible syntax frontend
    - MBaseLogic.dll: Prolog compiler
    - pfront.exe: PFront compiler
    - pfrepl.exe: PFront REPL
    - mlrepl.exe: MBaseML REPL
    - mlcomp.exe: MBaseML compiler
    - pfhighlight.exe: PFront emacs mode inferior process
    - prologrepl.exe: Prolog REPL

* Text editor:
    - misc/emacs/pfront.el: PFront mode for emacs


# CONTENTS (Sources)

* `src/cs/*`

    C# runtime library and a bootstrap tool

* `src/l/boot/*`

    Initial MBase bootstrap components

* `src/l/core/*`

    Core MBase compiler

* `src/l/ext/*`

    Core extensions

*  `src/l/lib/parsing/*`

    Packrat and Pratt parsing

*  `src/l/lib/wam/*`

    Embedded Prolog compiler

*  `src/l/lib/pfront/*`

    Extensible syntax frontend

*  `src/l/lib/ml/*`

    Embedded ML compiler

*  `src/l/lib/ssa/*`

    Generic SSA suppport and an SSA MBase compiler backend plugin

