# Fennel Cljlib

Experimental library for [Fennel](https://fennel-lang.org/) language, that adds many functions from [Clojure](https://clojure.org/)'s standard library.
This is not a one to one port of Clojure `core`, because many Clojure functions require certain guarantees, like immutability and laziness, which are hard to efficiently implement on top of Lua.
Some semantics like concurrency, or dynamic scope is not supported by Lua runtime at all.
Therefore certain functions were altered to better suit the domain.

## Installation

Clone library into your project or put it as a git submodule:

    $ git clone https://gitlab.com/andreyorst/fennel-cljlib cljlib

Now you can require `:cljlib` from Fennel:

``` clojure
(local clj (require :cljlib))
(import-macros cljm :cljlib.macros)
```

Optionally precompile the library to make it load slightly faster:

    $ cd cljlib; make

This will compile `init.fnl` into `init.lua`, so `require` should honor Lua files over Fennel files.
It is also possible to use this library from Lua this way.

## Documentation

Documentation is auto-generated with [Fenneldoc](https://gitlab.com/andreyorst/fenneldoc) and can be found [here](https://gitlab.com/andreyorst/fennel-cljlib/-/tree/master/doc).

# Contributing

Please make sure you've read [contribution guidelines](https://gitlab.com/andreyorst/fennel-cljlib/-/tree/master/CONTRIBUTING.md).

<!--  LocalWords:  Lua submodule precompile cljlib docstring config
      LocalWords:  namespace destructure
 -->
