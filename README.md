Purkka
======

Purkka is a low-level language, which compiles down to C. The aim is to provide
a refresh of C's syntax, macros and data structures without providing
additional bloat with run-time overhead.

From the Wiktionary English article for 'purkka':

**purkka**
- _(colloquial) chewing gum, bubble gum (type of candy)_
- _(music) bubblegum (pop music style)_
- _(slang) kludge, hack (temporary and/or unelegant solution)_

Usage
-----

    cargo check && cargo test

or something? The frontend isn't really working yet. Use the nightly compiler.

Goals
-----

- 100% compability with C
    - Supported by allowing inclusion of C files and compiling to C
    - Further target: compiling to maintainable C, but this requires a lot of
      work regarding macros in the output.
- As fast as C, as lightweight as C
    - If faced with cpu/memory tradeoffs, the resolution is to do it however C
      does it (which might mean not doing it). This means no in interfaces, no
      exceptions and the like, even if it would potentially speed up programs.
- A better macro system

Non-goals
---------

- Generics (or type classes, or traits with type parameters, or whatever)
- Interfaces
- Extensive standard library

Licence
-------

MIT, see LICENCE for details.
