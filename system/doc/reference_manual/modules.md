# Modules

## Module Syntax

Erlang code is divided into *modules*. A module consists of a sequence of attributes and function declarations, each terminated by period (.).

*Example:*

```text
-module(m).          % module attribute
-export([fact/1]).   % module attribute

fact(N) when N>0 ->  % beginning of function declaration
    N * fact(N-1);   %  |
fact(0) ->           %  |
    1.               % end of function declaration
```

For a description of function declarations, see [Function Declaration Syntax](ref_man_functions.md).

## Module Attributes

A *module attribute* defines a certain property of a module.

A module attribute consists of a tag and a value:

```text
-Tag(Value).
```

`Tag` must be an atom, while `Value` must be a literal term. As a convenience in user-defined attributes, if the literal term `Value` has the syntax `Name/Arity` (where `Name` is an atom and `Arity` a positive integer), the term `Name/Arity` is translated to `{Name,Arity}`.

Any module attribute can be specified. The attributes are stored in the compiled code and can be retrieved by calling `Module:module_info(attributes)`, or by using the module [beam_lib(3)](`beam_lib:chunks/2`) in STDLIB.

Several module attributes have predefined meanings. Some of them have arity two, but user-defined module attributes must have arity one.

### Pre-Defined Module Attributes

Pre-defined module attributes is to be placed before any function declaration.

* __`-module(Module).`__ - Module declaration, defining the name of the module. The name `Module`, an atom, is to be same as the file name minus the extension `.erl`. Otherwise [code loading](code_loading.md#loading) does not work as intended.

  This attribute is to be specified first and is the only mandatory attribute.

* __`-export(Functions).`__ - Exported functions. Specifies which of the functions, defined within the module, that are visible from outside the module.

  `Functions` is a list `[Name1/Arity1, ..., NameN/ArityN]`, where each `NameI` is an atom and `ArityI` an integer.

* __`-import(Module,Functions).`__ - Imported functions. Can be called the same way as local functions, that is, without any module prefix.

  `Module`, an atom, specifies which module to import functions from. `Functions` is a list similar as for `export`.

* __`-compile(Options).`__ - Compiler options. `Options` is a single option or a list of options. This attribute is added to the option list when compiling the module. See the `m:compile` manual page in Compiler.

* __`-vsn(Vsn).`__ - Module version. `Vsn` is any literal term and can be retrieved using `beam_lib:version/1`, see the [beam_lib(3)](`beam_lib:version/1`) manual page in STDLIB.

  If this attribute is not specified, the version defaults to the MD5 checksum of the module.

* __`-on_load(Function).`__ - This attribute names a function that is to be run automatically when a module is loaded. For more information, see [Running a Function When a Module is Loaded](code_loading.md#on_load).

* __`-nifs(Functions).`{: id=nifs_attribute }__ - Specifies which of the functions, defined within the module, that may be loaded as NIFs with `erlang:load_nif/2`.

  `Functions` is a list `[Name1/Arity1, ..., NameN/ArityN]`, where each `NameI` is an atom and `ArityI` an integer.

  While not strictly necessary, it is recommended to use `-nifs()` attribute in any module that load NIFs, to allow the compiler to make better decisions regarding optimizations.

  There is no need to add `-nifs([])` in modules that do not load NIFs. The lack of any call to `erlang:load_nif/2`, from within the module, is enough for the compiler to draw the same conclusion.

  > #### Change {: class=neutral }
  > The special meaning for the `-nifs()` attribute was introduced in Erlang/OTP 25.0. In previous releases, the `-nifs()` was accepted, but had no special meaning.

### Behaviour Module Attribute

It is possible to specify that the module is the callback module for a *behaviour*:

```text
-behaviour(Behaviour).
```

The atom `Behaviour` gives the name of the behaviour, which can be a user-defined behaviour or one of the following OTP standard behaviours:

* `gen_server`
* `gen_statem`
* `gen_event`
* `supervisor`

The spelling `behavior` is also accepted.

The callback functions of the module can be specified either directly by the exported function `behaviour_info/1`:

```text
behaviour_info(callbacks) -> Callbacks.
```

or by a `-callback` attribute for each callback function:

```text
-callback Name(Arguments) -> Result.
```

Here, `Arguments` is a list of zero or more arguments. The `-callback` attribute is to be preferred since the extra type information can be used by tools to produce documentation or find discrepancies.

Read more about behaviours and callback modules in [OTP Design Principles](`p:system:spec_proc.md#behaviours`).

### Record Definitions

The same syntax as for module attributes is used for record definitions:

```text
-record(Record,Fields).
```

Record definitions are allowed anywhere in a module, also among the function declarations. Read more in [Records](ref_man_records.md).

### Preprocessor

The same syntax as for module attributes is used by the preprocessor, which supports file inclusion, macros, and conditional compilation:

```text
-include("SomeFile.hrl").
-define(Macro,Replacement).
```

Read more in [Preprocessor](macros.md).

### Setting File and Line

The same syntax as for module attributes is used for changing the pre-defined macros `?FILE` and `?LINE`:

```text
-file(File, Line).
```

This attribute is used by tools, such as Yecc, to inform the compiler that the source program is generated by another tool. It also indicates the correspondence of source files to lines of the original user-written file, from which the source program is produced.

### Types and function specifications

A similar syntax as for module attributes is used for specifying types and function specifications:

```text
-type my_type() :: atom() | integer().
-spec my_function(integer()) -> integer().
```

Read more in [Types and Function specifications](typespec.md).

The description is based on [EEP8 - Types and function specifications](http://www.erlang.org/eeps/eep-0008.html), which is not to be further updated.

## The feature directive

While not a module attribute, but rather a directive (since it might affect syntax), there is the `-feature(..)` directive used for enabling and disabling [features](`p:system:features.md#features`).

The syntax is similar to that of an attribute, but has two arguments:

```text
-feature(FeatureName, enable | disable).
```

Note that the [feature directive](macros.md#feature-directive) can only appear in a prefix of the module.

## Comments

Comments can be placed anywhere in a module except within strings and quoted atoms. A comment begins with the character "%", continues up to, but does not include the next end-of-line, and has no effect. Notice that the terminating end-of-line has the effect of white space.

## module_info/0 and module_info/1 functions

The compiler automatically inserts the two special, exported functions into each module:

* `Module:module_info/0`
* `Module:module_info/1`

These functions can be called to retrieve information about the module.

### module_info/0

The `module_info/0` function in each module, returns a list of `{Key,Value}` tuples with information about the module. Currently, the list contain tuples with the following `Key`s: `module`, `attributes`, `compile`, `exports`, `md5` and `native`. The order and number of tuples may change without prior notice.

### module_info/1

The call `module_info(Key)`, where `Key` is an atom, returns a single piece of information about the module.

The following values are allowed for `Key`:

* __`module`__ - Returns an atom representing the module name.

* __`attributes`__ - Returns a list of `{AttributeName,ValueList}` tuples, where `AttributeName` is the name of an attribute, and `ValueList` is a list of values. Notice that a given attribute can occur more than once in the list with different values if the attribute occurs more than once in the module.

  The list of attributes becomes empty if the module is stripped with the [beam_lib(3)](`beam_lib:strip/1`) module (in STDLIB).

* __`compile`__ - Returns a list of tuples with information about how the module was compiled. This list is empty if the module has been stripped with the [beam_lib(3)](`beam_lib:strip/1`) module (in STDLIB).

* __`md5`__ - Returns a binary representing the MD5 checksum of the module.

* __`exports`__ - Returns a list of `{Name,Arity}` tuples with all exported functions in the module.

* __`functions`__ - Returns a list of `{Name,Arity}` tuples with all functions in the module.

* __`nifs`__ - Returns a list of `{Name,Arity}` tuples with all NIF functions in the module.

* __`native`__ - Return `true` if the module has native compiled code. Return `false` otherwise. In a system compiled without HiPE support, the result is always `false`
