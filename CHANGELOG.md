## Cljlib dev (???)

## Cljlib v1.1.0 (2022-10-29)

- Vectors can store `nil` values
- Add `pop` operation for vectors and sequences

## Cljlib v1.0.0 (2022-08-21)

Full rewrite of the library.
This library now requires the minimum Fennel version 1.2.0.
(Fennel v1.2.0 isn't yet released at the moment of cljlib v1.0.0 release, use the build from the `main` branch).

### New features

- added [lazy-seq](https://gitlab.com/andreyorst/lazy-seq) as a dependency, and made all sequence functions use it
- added [itable](https://gitlab.com/andreyorst/itable) as a dependency, and made all tables immutable by default
- implemented transducers
- implemented transients
- added `ns` macro

### Changes

- `fn*` no longer splits multisyms and doesn't bind both local and namespaced versions
- `fn*` is for anonymous functions, it doesn't create a local binding
- `def` now binds in terms of the namespace, specified by `ns`
- `def` no longer support documentation metadata

### Removed features

- removed `ordered-set` and `ordered-map`
- `into` is no longer a macro
- `empty` is no longer a macro
- removed metadata-related macros

### Fixes

- [#1](https://gitlab.com/andreyorst/fennel-cljlib/-/issues/1) has been fixed, and `fn*` now generates multi-value destructuring when possible.

## Cljlib v0.5.4 (2021-07-22)

- Remove `when-meta` macro.
- Add `loop` macro.
- Add `defn` as an alias to `fn*`.
- Make `fn*` always set the metadata.
- Fix `catch` in `try` not returning multiple values.

## Cljlib v0.5.3 (2021-04-24)

- test library is now external dependency.

## Cljlib v0.5.2 (2021-03-16)

- `conj` fix for better Luajit compatibility.

## Cljlib v0.5.1 (2021-02-19)

- `eq` will no longer change metamethods of tables.
- Module info is hidden in metatable now.
- `memoize` uses proper deep comparison.
- Tests no longer requires searching up in core namespace.
- Memoization test doesn't depend on CPU speed anymore.

## Cljlib v0.5.0 (2021-02-18)

- Greatly improved set iteration performance.
- Fix bug where `nil` was allowed as table key.

## Cljlib v0.4.0 (2021-02-15)

- `fn*` allows method definition via colon (`:`) syntax.
- Documentation reworked to contain documentation tests.
- `cljlib.fnl` renamed to `init.fnl`, library now uses relative requiring.
- Fix bug in `try` macro.

## Cljlib v0.3.0 (2020-12-31)

- New `hash-set` and `ordered-set` data structures.
- New `try` macro.

## Cljlib v0.2.0 (2020-11-18)

- Fix `seq` behavior for tables with numeric keys.
- Added `reduced` function
- `seq?` renamed to `vector?`
- Document library via [Fenneldoc](https://gitlab.com/andreyorst/fenneldoc).

## Cljlib v0.1.0 (2020-11-14)

First stable release of Fennel Cljlib.

<!-- LocalWords: Cljlib namespace Memoization metatable metamethods
     LocalWords:  arglist arity
 -->
