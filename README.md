# Fennel Cljlib

Experimental library for the [Fennel](https://fennel-lang.org/) language, that adds many functions from [Clojure](https://clojure.org/)'s standard library.
This is not a one-to-one port of Clojure `core`, because many Clojure features require certain facilities from the runtime.
This library implements lazy sequences, transducers, immutable tables, sets and vectors, transients, and a lot of functions from the `core` namespace.
Some semantics like concurrency, or dynamic scope is not supported by Lua runtime at all.
Therefore, certain functions were altered to better suit the domain.

## Installation

Clone library into your project or put it as a git submodule:

    $ git clone --recursive https://gitlab.com/andreyorst/fennel-cljlib club

Now you can require `:cljlib` from Fennel:

``` fennel
(local clj (require :cljlib))
(import-macros cljm :cljlib)
```

Make sure to set up the `FENNEL_PATH` and `LUA_PATH` to include the installation directory:

    FENNEL_PATH="cljlib/?/init.fnl;$FENNEL_PATH"
    LUA_PATH="cljlib/?/init.lua;$LUA_PATH"

Alternatively, precompile the library to make it load slightly faster:

    $ cd cljlib; make

This will compile `init.fnl` into the `cljlib.lua` file, with all dependencies included.
It is also possible to use this library directly from Lua this way.

## Documentation

Documentation is auto-generated with [Fenneldoc](https://gitlab.com/andreyorst/fenneldoc) and can be found [here](https://gitlab.com/andreyorst/fennel-cljlib/-/tree/master/doc).

# Contributing

Please make sure you've read [contribution guidelines](https://gitlab.com/andreyorst/fennel-cljlib/-/tree/master/CONTRIBUTING.md).

<!--  LocalWords:  Lua submodule precompile cljlib docstring config
      LocalWords:  namespace destructure runtime Clojure
 -->
