Purkka
======

Purkka is a low-level language, which compiles down to C. The aim is to provide
a refresh of C's syntax, macros and data structures without providing
additional bloat with run-time overhead.

From the Wiktionary English article for 'purkka':

**purkka**
- _(colloquial) chewing gum, bubble gum (type of candy)_
- _(music) bubblegum (pop music style)_
- **(slang) kludge, hack (temporary and/or unelegant solution)**

Usage
-----

    cargo build && cargo test

There's a handful of testcases in src/purkka/tests/testcases, you test the
compiler with those.

    ./target/debug/purkka src/purkka/tests/testcases/main.prk

Goals
-----

- 100% compability with C
    - Supported by allowing inclusion of C files and compiling to C
- As fast as C, as lightweight as C
    - If faced with cpu/memory tradeoffs, the resolution is to do it however C
      does it (which probably means not doing it). This means no in interfaces,
      no exceptions and the like, even if it would potentially speed up
      programs.
- At least syntactic improvements for C

Future goals
------------

- Complete support for everything the compiler claims to support
    - grep for "implemented" in the source code to find to-do stuff
- Implementations for sum types and tuples
- Fixing type system to be stricter in all cases except expanded C macros
- Better macro system than C

Non-goals
---------

- Generics (or type classes, or traits with type parameters, or whatever)
- Interfaces
- Standard library

Licence
-------

MIT, see LICENCE for details.
