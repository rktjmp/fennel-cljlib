Please read the following document to make collaborating on the project easier for both sides.

# Reporting bugs

If you've encountered a bug, do the following:

-   Check if the documentation has information about the problem you have.
    Maybe this isn't a bug, but a desired behavior.
-   Check issue tracker, maybe someone had reported your problem already.
    If there's no issue, describing your problem, or there is, but it is closed, please create new issue, and link all closed issues that relate to this problem, if any.
-   Tag issue with a `BUG:` at the beginning of the issue name.

# Suggesting features and/or changes

Before suggesting a feature, please check if this feature wasn't requested before.
You can do that in the issues, by filtering issues by `FEATURE:`.
If no feature found, please file new issue, and tag it with a `FEATURE:` at the beginning of the issue name.

# Contributing changes

Please do.

When deciding to contribute a large amount of changes, first consider opening a `DISCUSSION:` type issue, so we could first decide if such dramatic changes are in the scope of the project.
This will save your time, in case such changes are out of the project's scope.

If you're contributing a bug-fix, please open a `BUG:` labeled issue first, unless someone already did that.
All bug related merge requests must have a linked issues with a meaningful explanation and steps of reproducing a bug.
Small fixes are also welcome, and doesn't require filing an issue, although you may be asked to do so.

## Writing code

When writing code, consider following the existing style without applying dramatic changes to formatting unless really necessary.
For this particular project, please follow rules as described in [Clojure Style Guide](https://github.com/bbatsov/clojure-style-guide).
If you see any inconsistencies with the style guide in the code, feel free to change these in a non-breaking way.
If you're using Emacs, some indentation rules are predefined in `.dir-locals.el` file, so when adding new macro please add a meaningful indentation spec to that file as well.

If you've added new functions, each must be covered with a set of tests.
For that purpose this project has special `test.fnl` module, that defines such macros as `assert-is`, `assert-not`, `assert-eq`, `assert-ne`, `deftest`, and `testing`.
Related tests should be grouped with the `deftest` macro, which defines a meaningful name for the test, and test itself must be defined within `testing` macros.
All assertions in tests must be one with one of `assert-eq`, `assert-ne`, `assert-not`, or `assert-is` macros, as these provide human readable output in the log.

When changing existing functions make sure that all tests pass.
If some tests do not pass, make sure that these tests are written to test this particular function.
If the breakage was expected (e.g. when contributing a breaking change), make sure to update the tests.
If neither from above applies, then, perhaps, you've broke something horribly.

Before committing changes you must run tests with `make test`, and all of the tests must pass without errors.
Consider checking test coverage with `make luacov` and rendering it with your preferred reporter.
Makefile also has `luacov-console` target, which can be used to see coverage of Lua code directly in the terminal with [luacov-console](https://github.com/spacewander/luacov-console).
Coverage should not drop too much, and huge drops usually mean that tests should cover more input variants.

## Writing documentation

If you've added new code, make sure it is covered not only by tests but also with documentation.
This includes writing documentation strings directly in the code, by using docstring feature of the language.
Documentation files are auto-generated with [Fenneldoc](https://gitlab.com/andreyorst/fenneldoc), so please refer to its documentation for available features.

General notes on writing documentation:

- Please consider using spell checking.
  If you find a word not known by the dictionary, please add it to the `LocalWords` section at the bottom of the document.
- If you're writing markdown by hand, please consider using one sentence per line approach.
  This makes it easier to reason about text in patches, and Markdown ignores single newlines in sentences.
- If you're writing documentation for function, consider splitting lines at column 80.

## Working with Git

Check out new branch from project's main development branch.
If you've cloned this project some time ago, consider checking if your branch has all recent changes from upstream.

Each commit must have a type, which is one of `feature`, `fix`, followed by optional scope, and a must have description after `:` colon.
For example:

    fix(core macros): fix #42
    feature(tests): add more strict tests

-   `feature` must be used when adding new code.
-   `fix` must be used when working with existing code.

When creating merge request consider squashing your commits at merge.
You may do this manually, or use Gitlab's "Squash commits" button.
Either way please tag squash commit with `fix` or `feature`, depending on what you're willing to merge.

<!--  LocalWords:  Makefile Gitlab's Lua docstring
 -->
