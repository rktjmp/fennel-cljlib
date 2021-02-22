# Test.fnl

**Table of contents**

- [`deftest`](#deftest)
- [`testing`](#testing)
- [`assert-eq`](#assert-eq)
- [`assert-ne`](#assert-ne)
- [`assert-is`](#assert-is)
- [`assert-not`](#assert-not)

## `deftest`
Function signature:

```
(deftest name ...)
```

Simple way of grouping tests with `name`.

## `testing`
Function signature:

```
(testing description ...)
```

Print test `description` and run it.

## `assert-eq`
Function signature:

```
(assert-eq expr1 expr2 msg)
```

Like `assert`, except compares results of `expr1` and `expr2` for equality.
Generates formatted message if `msg` is not set to other message.

### Example
Compare two expressions:

``` fennel
;; (assert-eq 1 (+1 1))
;; => runtime error: equality assertion failed
;; =>   Left: 1
;; =>   Right: 3
```

Deep compare values:

``` fennel
;; (assert-eq [1 {[2 3] [4 5 6]}] [1 {[2 3] [4 5]}])
;; => runtime error: equality assertion failed
;; =>   Left: [1 {[2 3] [4 5 6]}]
;; =>   Right: [1 {[2 3] [4 5]}]
```

## `assert-ne`
Function signature:

```
(assert-ne expr1 expr2 msg)
```

Assert for unequality.  Like `assert`, except compares results of
`expr1` and `expr2` for equality.  Generates formatted message if
`msg` is not set to other message.  Same as [`assert-eq`](#assert-eq).

## `assert-is`
Function signature:

```
(assert-is expr msg)
```

Assert `expr` for truth. Same as inbuilt `assert`, except generates more
  verbose message if `msg` is not set.

``` fennel
;; (assert-is (= 1 2 3))
;; => runtime error: assertion failed for (= 1 2 3)
```

## `assert-not`
Function signature:

```
(assert-not expr msg)
```

Assert `expr` for not truth. Generates more verbose message if
  `msg` is not set. Works the same as [`assert-is`](#assert-is).


<!-- Generated with Fenneldoc v0.1.3
     https://gitlab.com/andreyorst/fenneldoc -->
