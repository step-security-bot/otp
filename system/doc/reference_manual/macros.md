# Preprocessor

## File Inclusion

A file can be included as follows:

```text
-include(File).
-include_lib(File).
```

`File`, a string, is to point out a file. The contents of this file are included as is, at the position of the directive.

Include files are typically used for record and macro definitions that are shared by several modules. It is recommended to use the file name extension `.hrl` for include files.

`File` can start with a path component `$VAR`, for some string `VAR`. If that is the case, the value of the environment variable `VAR` as returned by `os:getenv(VAR)` is substituted for `$VAR`. If `os:getenv(VAR)` returns `false`, `$VAR` is left as is.

If the filename `File` is absolute (possibly after variable substitution), the include file with that name is included. Otherwise, the specified file is searched for in the following directories, and in this order:

1. The current working directory
1. The directory where the module is being compiled
1. The directories given by the `include` option

For details, see the [erlc(1)](`p:erts:erlc_cmd.md`) manual page in ERTS and `m:compile` manual page in Compiler.

*Examples:*

```text
-include("my_records.hrl").
-include("incdir/my_records.hrl").
-include("/home/user/proj/my_records.hrl").
-include("$PROJ_ROOT/my_records.hrl").
```

`include_lib` is similar to `include`, but is not to point out an absolute file. Instead, the first path component (possibly after variable substitution) is assumed to be the name of an application.

*Example:*

```text
-include_lib("kernel/include/file.hrl").
```

The code server uses `code:lib_dir(kernel)` to find the directory of the current (latest) version of Kernel, and then the subdirectory `include` is searched for the file `file.hrl`.

## Defining and Using Macros

A macro is defined as follows:

```text
-define(Const, Replacement).
-define(Func(Var1,...,VarN), Replacement).
```

A macro definition can be placed anywhere among the attributes and function declarations of a module, but the definition must come before any usage of the macro.

If a macro is used in several modules, it is recommended that the macro definition is placed in an include file.

A macro is used as follows:

```text
?Const
?Func(Arg1,...,ArgN)
```

Macros are expanded during compilation. A simple macro `?Const` is replaced with `Replacement`.

*Example:*

```text
-define(TIMEOUT, 200).
...
call(Request) ->
    server:call(refserver, Request, ?TIMEOUT).
```

This is expanded to:

```text
call(Request) ->
    server:call(refserver, Request, 200).
```

A macro `?Func(Arg1,...,ArgN)` is replaced with `Replacement`, where all occurrences of a variable `Var` from the macro definition are replaced with the corresponding argument `Arg`.

*Example:*

```text
-define(MACRO1(X, Y), {a, X, b, Y}).
...
bar(X) ->
    ?MACRO1(a, b),
    ?MACRO1(X, 123)
```

This is expanded to:

```text
bar(X) ->
    {a,a,b,b},
    {a,X,b,123}.
```

It is good programming practice, but not mandatory, to ensure that a macro definition is a valid Erlang syntactic form.

To view the result of macro expansion, a module can be compiled with the `'P'` option. `compile:file(File, ['P'])`. This produces a listing of the parsed code after preprocessing and parse transforms, in the file `File.P`.

## Predefined Macros

The following macros are predefined:

* __`?MODULE`__ - The name of the current module.

* __`?MODULE_STRING`.__ - The name of the current module, as a string.

* __`?FILE`.__ - The file name of the current module.

* __`?LINE`.__ - The current line number.

* __`?MACHINE`.__ - The machine name, `'BEAM'`.

* __`?FUNCTION_NAME`__ - The name of the current function.

* __`?FUNCTION_ARITY`__ - The arity (number of arguments) for the current function.

* __`?OTP_RELEASE`__ - The OTP release that the currently executing ERTS application is part of, as an integer. For details, see [`erlang:system_info(otp_release)`](`erlang:system_info/1`).
  > #### Change {: class=neutral }
  > The `?OTP_RELEASE` macro was introduced in Erlang/OTP 21.

* __`?FEATURE_AVAILABLE(Feature)`__ - Expands to `true` if the [feature](`p:system:features.md#features`) `Feature` is available. The feature might or might not be enabled.
  > #### Change {: class=neutral }
  > The `?FEATURE_AVAILABLE()` macro was introduced in Erlang/OTP 25.

* __`?FEATURE_ENABLED(Feature)`__ - Expands to `true` if the [feature](`p:system:features.md#features`) `Feature` is enabled.
  > #### Change {: class=neutral }
  > The `?FEATURE_ENABLED()` macro was introduced in Erlang/OTP 25.

## Macros Overloading

It is possible to overload macros, except for predefined macros. An overloaded macro has more than one definition, each with a different number of arguments.

> #### Change {: class=neutral }
> Support for overloading of macros was added in Erlang 5.7.5/OTP R13B04.

A macro `?Func(Arg1,...,ArgN)` with a (possibly empty) list of arguments results in an error message if there is at least one definition of `Func` with arguments, but none with N arguments.

Assuming these definitions:

```text
-define(F0(), c).
-define(F1(A), A).
-define(C, m:f).
```

the following does not work:

```text
f0() ->
    ?F0. % No, an empty list of arguments expected.

f1(A) ->
    ?F1(A, A). % No, exactly one argument expected.
```

On the other hand,

```text
f() ->
    ?C().
```

is expanded to

```text
f() ->
    m:f().
```

## Removing a macro definition

A definition of macro can be removed as follows:

```text
-undef(Macro).
```

## Conditional Compilation

The following macro directives support conditional compilation:

* __`-ifdef(Macro).`__ - Evaluate the following lines only if `Macro` is defined.

* __`-ifndef(Macro).`__ - Evaluate the following lines only if `Macro` is not defined.

* __`-else.`__ - Only allowed after the `ifdef`, `ifndef`, `if`, and `elif` directives. The lines following `else` are evaluated if the preceding directive evaluated to false.

* __`-if(Condition).`__ - Evaluates the following lines only if `Condition` evaluates to true.

* __`-elif(Condition).`__ - Only allowed after an `if` or another `elif` directive. If the preceding `if` or `elif` directive does not evaluate to true, and the `Condition` evaluates to true, the lines following the `elif` are evaluated instead.

* __`-endif.`__ - Specifies the end of a series of control flow directives.

> #### Note {: class=info }
> Macro directives cannot be used inside functions.

Syntactically, the `Condition` in `if` and `elif` must be a [guard expression](expressions.md#guard-expressions). Other constructs (such as a `case` expression) result in a compilation error.

As opposed to the standard guard expressions, an expression in an `if` and `elif` also supports calling the psuedo-function `defined(Name)`, which tests whether the `Name` argument is the name of a previously defined macro. `defined(Name)` evaluates to `true` if the macro is defined and `false` otherwise. An attempt to call other functions results in a compilation error.

*Example:*

```text
-module(m).
...

-ifdef(debug).
-define(LOG(X), io:format("{~p,~p}: ~p~n", [?MODULE,?LINE,X])).
-else.
-define(LOG(X), true).
-endif.

...
```

When trace output is desired, `debug` is to be defined when the module `m` is compiled:

```text
% erlc -Ddebug m.erl

or

1> c(m, {d, debug}).
{ok,m}
```

`?LOG(Arg)` is then expanded to a call to `io:format/2` and provide the user with some simple trace output.

*Example:*

```text
-module(m)
...
-if(?OTP_RELEASE >= 25).
%% Code that will work in OTP 25 or higher
-elif(?OTP_RELEASE >= 26).
%% Code that will work in OTP 26 or higher
-else.
%% Code that will work in OTP 24 or lower.
-endif.
...
```

This code uses the `OTP_RELEASE` macro to conditionally select code depending on release.

*Example:*

```text
-module(m)
...
-if(?OTP_RELEASE >= 26 andalso defined(debug)).
%% Debugging code that requires OTP 26 or later.
-else.
%% Non-debug code that works in any release.
-endif.
...
```

This code uses the `OTP_RELEASE` macro and `defined(debug)` to compile debug code only for OTP 26 or later.

## The -feature() directive

[](){: id=feature-directive }
The directive `-feature(FeatureName, enable | disable)` can be used to enable or disable the [feature](`p:system:features.md#features`) `FeatureName`. This is the preferred way of enabling (disabling) features, although it is possible to do it with options to the compiler as well.

Note that the `-feature(..)` directive may only appear before any syntax is used. In practice this means it should appear before any `-export(..)` or record definitions.

## \-error() and -warning() directives

The directive `-error(Term)` causes a compilation error.

*Example:*

```text
-module(t).
-export([version/0]).

-ifdef(VERSION).
version() -> ?VERSION.
-else.
-error("Macro VERSION must be defined.").
version() -> "".
-endif.
```

The error message will look like this:

```text
% erlc t.erl
t.erl:7: -error("Macro VERSION must be defined.").
```

The directive `-warning(Term)` causes a compilation warning.

*Example:*

```text
-module(t).
-export([version/0]).

-ifndef(VERSION).
-warning("Macro VERSION not defined -- using default version.").
-define(VERSION, "0").
-endif.
version() -> ?VERSION.
```

The warning message will look like this:

```text
% erlc t.erl
t.erl:5: Warning: -warning("Macro VERSION not defined -- using default version.").
```

> #### Change {: class=neutral }
> The `-error()` and `-warning()` directives were added in Erlang/OTP 19.

## Stringifying Macro Arguments

The construction `??Arg`, where `Arg` is a macro argument, is expanded to a string containing the tokens of the argument. This is similar to the `#arg` stringifying construction in C.

*Example:*

```text
-define(TESTCALL(Call), io:format("Call ~s: ~w~n", [??Call, Call])).

?TESTCALL(myfunction(1,2)),
?TESTCALL(you:function(2,1)).
```

results in

```text
io:format("Call ~s: ~w~n",["myfunction ( 1 , 2 )",myfunction(1,2)]),
io:format("Call ~s: ~w~n",["you : function ( 2 , 1 )",you:function(2,1)]).
```

That is, a trace output, with both the function called and the resulting value.
