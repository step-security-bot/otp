# assert.hrl

Assert macros.

## Description

The include file `assert.hrl` provides macros for inserting assertions in your program code.

Include the following directive in the module from which the function is called:

```text
-include_lib("stdlib/include/assert.hrl").
```

When an assertion succeeds, the assert macro yields the atom `ok`. When an assertion fails, an exception of type `error` is generated. The associated error term has the form `{Macro, Info}`. `Macro` is the macro name, for example, `assertEqual`. `Info` is a list of tagged values, such as `[{module, M}, {line, L}, ...]`, which gives more information about the location and cause of the exception. All entries in the `Info` list are optional; do not rely programmatically on any of them being present.

Each assert macro has a corresponding version with an extra argument, for adding comments to assertions. These can for example be printed as part of error reports, to clarify the meaning of the check that failed. For example, `?assertEqual(0, fib(0), "Fibonacci is defined for zero")`. The comment text can be any character data (string, UTF8-binary, or deep list of such data), and will be included in the error term as `{comment, Text}`.

If the macro `NOASSERT` is defined when `assert.hrl` is read by the compiler, the macros are defined as equivalent to the atom `ok`. The test will not be performed and there is no cost at runtime.

For example, using `erlc` to compile your modules, the following disables all assertions:

```text
erlc -DNOASSERT=true *.erl
```

(The value of `NOASSERT` does not matter, only the fact that it is defined.)

A few other macros also have effect on the enabling or disabling of assertions:

* If `NODEBUG` is defined, it implies `NOASSERT` (unless `DEBUG` is also defined, which overrides `NODEBUG`).
* If `ASSERT` is defined, it overrides `NOASSERT`, that is, the assertions remain enabled.

If you prefer, you can thus use only `DEBUG`/`NODEBUG` as the main flags to control the behavior of the assertions (which is useful if you have other compiler conditionals or debugging macros controlled by those flags), or you can use `ASSERT`/`NOASSERT` to control only the assert macros.

## Macros

* __`assert(BoolExpr)`__  

* __`assert(BoolExpr, Comment)`__ - Tests that `BoolExpr` completes normally returning `true`.

* __`assertNot(BoolExpr)`__  

* __`assertNot(BoolExpr, Comment)`__ - Tests that `BoolExpr` completes normally returning `false`.

* __`assertMatch(GuardedPattern, Expr)`__  

* __`assertMatch(GuardedPattern, Expr, Comment)`__ - Tests that `Expr` completes normally yielding a value that matches `GuardedPattern`, for example:

  ```text
  ?assertMatch({bork, _}, f())
  ```

  Notice that a guard `when ...` can be included:

  ```text
  ?assertMatch({bork, X} when X > 0, f())
  ```

* __`assertNotMatch(GuardedPattern, Expr)`__  

* __`assertNotMatch(GuardedPattern, Expr, Comment)`__ - Tests that `Expr` completes normally yielding a value that does not match `GuardedPattern`.

  As in `assertMatch`, `GuardedPattern` can have a `when` part.

* __`assertEqual(ExpectedValue, Expr)`__  

* __`assertEqual(ExpectedValue, Expr, Comment)`__ - Tests that `Expr` completes normally yielding a value that is exactly equal to `ExpectedValue`.

* __`assertNotEqual(ExpectedValue, Expr)`__  

* __`assertNotEqual(ExpectedValue, Expr, Comment)`__ - Tests that `Expr` completes normally yielding a value that is not exactly equal to `ExpectedValue`.

* __`assertException(Class, Term, Expr)`__  

* __`assertException(Class, Term, Expr, Comment)`__ - Tests that `Expr` completes abnormally with an exception of type `Class` and with the associated `Term`. The assertion fails if `Expr` raises a different exception or if it completes normally returning any value.

  Notice that both `Class` and `Term` can be guarded patterns, as in `assertMatch`.

* __`assertNotException(Class, Term, Expr)`__  

* __`assertNotException(Class, Term, Expr, Comment)`__ - Tests that `Expr` does not evaluate abnormally with an exception of type `Class` and with the associated `Term`. The assertion succeeds if `Expr` raises a different exception or if it completes normally returning any value.

  As in `assertException`, both `Class` and `Term` can be guarded patterns.

* __`assertError(Term, Expr)`__  

* __`assertError(Term, Expr, Comment)`__ - Equivalent to `assertException(error, Term, Expr)`

* __`assertExit(Term, Expr)`__  

* __`assertExit(Term, Expr, Comment)`__ - Equivalent to `assertException(exit, Term, Expr)`

* __`assertThrow(Term, Expr)`__  

* __`assertThrow(Term, Expr, Comment)`__ - Equivalent to `assertException(throw, Term, Expr)`

## See Also

`m:compile`, [`erlc(3)`](`p:erts:erlc_cmd.md`)
