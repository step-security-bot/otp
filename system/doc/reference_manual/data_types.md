# Data Types

Erlang provides a number of data types, which are listed in this section.

[](){: id=no_user_types }
Note that Erlang has no user defined types, only composite types (data structures) made of Erlang terms. This means that any function testing for a composite type, typically named `is_type/1`, might return `true` for a term that coincides with the chosen representation. The corresponding functions for built in types do not suffer from this.

## Terms

A piece of data of any data type is called a *term*.

## Number

There are two types of numeric literals, *integers* and *floats*. Besides the conventional notation, there are two Erlang-specific notations:

* `$`*`char`*  
  ASCII value or unicode code-point of the character *`char`*.
* *`base`*`#`*`value`*  
  Integer with the base *`base`*, that must be an integer in the range 2..36.

Leading zeroes are ignored. Single underscore `_` can be inserted between digits as a visual separator.

*Examples:*

```text
1> 42.
42
2> -1_234_567_890.
-1234567890
3> $A.
65
4> $\n.
10
5> 2#101.
5
6> 16#1f.
31
7> 16#4865_316F_774F_6C64.
5216630098191412324
8> 2.3.
2.3
9> 2.3e3.
2.3e3
10> 2.3e-3.
0.0023
11> 1_234.333_333
1234.333333
```

[](){: id=numeric_comparisons }
### Comparisons

Both integers and floats share the same linear order. That is, `1` compares less than `2.4`, `3` compares greater than `2.99999`, and `5` is equal to `5.0`.

When wanting to compare an integer with another integer or a float with another float, it may be tempting to use the term equivalence operators (`=:=`, `=/=`) or pattern matching. This works for integers which has a distinct representation for every number, but there's a surprising edge case for floating-point as the latter has two representations for zero which are considered different by the term equivalence operators and pattern matching.

If you wish to compare floating-point numbers *numerically*, use the regular comparison operators (such as `==`) and add guards that require both the arguments to be floating-point.

> #### Note {: class=info }
> Prior to OTP 27, the term equivalence operators had a bug where they considered `0.0` and `-0.0` to be the same term. Legacy code that makes equality comparisons on floating-point zero should migrate to using the equal-to (`==`) operator with `is_float/1` guards, and compiler warnings have been added to that effect. These can be silenced by writing `+0.0` instead, which is the same as `0.0` but makes the compiler interpret the comparison as being purposely made against `0.0`.
>
> Note that this does *not* break compatibility with IEEE 754 which mandates that `0.0` and `-0.0` should compare equal: they are equal when interpreted as numbers (`==`), and unequal when interpreted as opaque terms (`=:=`).

[](){: id=float_representation_problem }
### Representation of Floating Point Numbers

When working with floats you may not see what you expect when printing or doing arithmetic operations. This is because floats are represented by a fixed number of bits in a base-2 system while printed floats are represented with a base-10 system. Erlang uses 64-bit floats. Here are examples of this phenomenon:

```text
> 0.1+0.2.
0.30000000000000004
```

The real numbers `0.1` and `0.2` cannot be represented exactly as floats.

```text
> {36028797018963968.0, 36028797018963968 == 36028797018963968.0,
  36028797018963970.0, 36028797018963970 == 36028797018963970.0}.
{3.602879701896397e16, true,
 3.602879701896397e16, false}.
```

The value `36028797018963968` can be represented exactly as a float value but Erlang's pretty printer rounds `36028797018963968.0` to `3.602879701896397e16` (`=36028797018963970.0`) as all values in the range `[36028797018963966.0, 36028797018963972.0]` are represented by `36028797018963968.0`.

For more information about floats and issues with them see:

* [What Every Programmer Should Know About Floating-Point Arithmetic](https://floating-point-gui.de/),
* [0\.30000000000000004.com/](https://0.30000000000000004.com/), and
* [Floating Point Arithmetic: Issues and Limitations](https://docs.python.org/3/tutorial/floatingpoint.html).

If you need to work with decimal fractions, for instance if you need to represent money, then you should use a library that handles that or work in cents instead of euros so that you do not need decimal fractions.

Please also note that Erlang's floats do not exactly match IEEE 754 floats, in that neither Inf nor NaN are supported in Erlang. Any operation that would result in NaN, +Inf, or -Inf, will instead raise a `badarith` exception.

## Atom

An atom is a literal, a constant with name. An atom is to be enclosed in single quotes (') if it does not begin with a lower-case letter or if it contains other characters than alphanumeric characters, underscore (_), or @.

*Examples:*

```text
hello
phone_number
'Monday'
'phone number'
```

## Bit Strings and Binaries

A bit string is used to store an area of untyped memory.

Bit strings are expressed using the [bit syntax](expressions.md#bit_syntax).

Bit strings that consist of a number of bits that are evenly divisible by eight, are called *binaries*

*Examples:*

```text
1> <<10,20>>.
<<10,20>>
2> <<"ABC">>.
<<"ABC">>
1> <<1:1,0:1>>.
<<2:2>>
```

For more examples, see [Programming Examples](`p:system:bit_syntax.md`).

## Reference

A term that is [unique](`p:system:advanced.md#unique_references`) among connected nodes. A reference can be created by calling the [`make_ref/0`](`erlang:make_ref/0`) BIF. The [`is_reference/1`](`erlang:is_reference/1`) BIF can be used to test if a term is a reference.

## Fun

A fun is a functional object. Funs make it possible to create an anonymous function and pass the function itself -- not its name -- as argument to other functions.

*Example:*

```text
1> Fun1 = fun (X) -> X+1 end.
#Fun<erl_eval.6.39074546>
2> Fun1(2).
3
```

Read more about funs in [Fun Expressions](expressions.md#funs). For more examples, see [Programming Examples](`p:system:funs.md`).

## Port Identifier

A port identifier identifies an Erlang port.

`open_port/2`, which is used to create ports, returns a value of this data type.

Read more about ports in [Ports and Port Drivers](ports.md).

## PID

PID is an abbreviation for process identifier. Each process has a PID which identifies the process. PIDs are unique among processes that are alive on connected nodes. However, a PID of a terminated process may be reused as a PID for a new process after a while.

The BIF [`self/0`](`erlang:self/0`) returns the PID of the calling process. When [creating a new process](ref_man_processes.md#process-creation), the parent process will be able to get the PID of the child process either via the return value, as is the case when calling the [`spawn/3`](`erlang:spawn/3`) BIF, or via a message, which is the case when calling the [`spawn_request/5`](`erlang:spawn_request/5`) BIF. A PID is typically used when when sending a process a [signal](ref_man_processes.md#signals). The [`is_pid/1`](`erlang:is_pid/1`) BIF can be used to test whether a term is a PID.

*Example:*

```text
-module(m).
-export([loop/0]).

loop() ->
    receive
        who_are_you ->
            io:format("I am ~p~n", [self()]),
            loop()
    end.

1> P = spawn(m, loop, []).
<0.58.0>
2> P ! who_are_you.
I am <0.58.0>
who_are_you
```

Read more about processes in [Processes](ref_man_processes.md).

## Tuple

A tuple is a compound data type with a fixed number of terms:

```text
{Term1,...,TermN}
```

Each term `Term` in the tuple is called an *element*. The number of elements is said to be the *size* of the tuple.

There exists a number of BIFs to manipulate tuples.

*Examples:*

```text
1> P = {adam,24,{july,29}}.
{adam,24,{july,29}}
2> element(1,P).
adam
3> element(3,P).
{july,29}
4> P2 = setelement(2,P,25).
{adam,25,{july,29}}
5> tuple_size(P).
3
6> tuple_size({}).
0
```

## Map

A map is a compound data type with a variable number of key-value associations:

```text
#{Key1=>Value1,...,KeyN=>ValueN}
```

Each key-value association in the map is called an *association pair*. The key and value parts of the pair are called *elements*. The number of association pairs is said to be the *size* of the map.

There exists a number of BIFs to manipulate maps.

*Examples:*

```text
1> M1 = #{name=>adam,age=>24,date=>{july,29}}.
#{age => 24,date => {july,29},name => adam}
2> maps:get(name,M1).
adam
3> maps:get(date,M1).
{july,29}
4> M2 = maps:update(age,25,M1).
#{age => 25,date => {july,29},name => adam}
5> map_size(M).
3
6> map_size(#{}).
0
```

A collection of maps processing functions can be found in `m:maps` manual page in STDLIB.

Read more about maps in [Map Expressions](expressions.md#map_expressions).

> #### Change {: class=neutral }
> Maps were introduced as an experimental feature in Erlang/OTP R17. Their functionality was extended and became fully supported in Erlang/OTP 18.

## List

A list is a compound data type with a variable number of terms.

```text
[Term1,...,TermN]
```

Each term `Term` in the list is called an *element*. The number of elements is said to be the *length* of the list.

Formally, a list is either the empty list `[]` or consists of a *head* (first element) and a *tail* (remainder of the list). The *tail* is also a list. The latter can be expressed as `[H|T]`. The notation `[Term1,...,TermN]` above is equivalent with the list `[Term1|[...|[TermN|[]]]]`.

*Example:*

`[]` is a list, thus  
`[c|[]]` is a list, thus  
`[b|[c|[]]]` is a list, thus  
`[a|[b|[c|[]]]]` is a list, or in short `[a,b,c]`

A list where the tail is a list is sometimes called a *proper list*. It is allowed to have a list where the tail is not a list, for example, `[a|b]`. However, this type of list is of little practical use.

*Examples:*

```text
1> L1 = [a,2,{c,4}].
[a,2,{c,4}]
2> [H|T] = L1.
[a,2,{c,4}]
3> H.
a
4> T.
[2,{c,4}]
5> L2 = [d|T].
[d,2,{c,4}]
6> length(L1).
3
7> length([]).
0
```

A collection of list processing functions can be found in the `m:lists` manual page in STDLIB.

## String

Strings are enclosed in double quotes ("), but is not a data type in Erlang. Instead, a string `"hello"` is shorthand for the list `[$h,$e,$l,$l,$o]`, that is, `[104,101,108,108,111]`.

Two adjacent string literals are concatenated into one. This is done in the compilation, thus, does not incur any runtime overhead.

*Example:*

```text
"string" "42"
```

is equivalent to

```text
"string42"
```

Strings can also be written as *triple-quoted strings*, which can be *indented* over multiple lines to follow the indentation of the surrounding code. They are also *verbatim*, that is; they don't allow escape sequences, and thereby don't need double quote characters to be escaped.

> #### Change {: class=neutral }
> Triple-quoted strings were added in Erlang/OTP 27. Before that 3 consecutive double quote characters had a different meaning. There were absolutely no good reason to write such a character sequence before triple-quoted strings existed, but there *are* some gotcha:s; see the [Warning ](data_types.md#triple-quoted-strings-warning)at the end of this description of triple-quoted strings.

Example, with verbatim double quote characters:

```text
"""
  Line "1"
  Line "2"
  """
```

That is equivalent to the normal single quoted string (which also allows newlines):

```text
"Line \"1\"
Line \"2\""
```

The opening and the closing line has got the delimiters; the `"""` characters. The lines between them are the content lines. The newline on the opening line is not regarded as string content, nor is the newline on the last content line.

The indentation is defined by the white space character sequence preceding the delimiter on the closing line. That character sequence is stripped from all content lines. There can only be white space before the delimiter on the closing line, or else it is regarded as a content line.

The opening line is not allowed to have any characters other than white space after the delimiter, and all content lines must start with the defined indentation character sequence, otherwise the string has a syntax error.

Here is a larger example:

```text
X = """
      First line starting with two spaces
    Not escaped: "\t \r \xFF" and """
    
    """
```

That corresponds to the normal string:

```text
X = "  First line starting with two spaces
Not escaped: \"\\t \\r \\xFF\" and \"\"\"
"
```

It is possible to write consecutive double quote characters on the beginning of a content line by using more double quote characters as delimiters. This is a string that contains exactly 4 double quote characters, using a delimiter with 5 double quote characters:

```text
"""""
""""
"""""
```

These strings are all the empty string:

```text
""
```

```text
"""
"""
```

```text
"""
  
  """
```

[](){: id=triple-quoted-strings-warning }
> #### Warning {: class=warning }
> Before Erlang/OTP 27, when triple-quoted strings were added, the character sequence `"""` was interpreted as `"" "` which means concatenate the empty string to the string that follows. All sequences of an odd number of double quote characters had this meaning.
>
> Any even number of double quote characters was interpreted as a sequence of empty strings, that were concatenated (to the empty string).
>
> There was no reason to write such character sequences. But should that have happended; the meaning probably changed with the introduction of triple-quoted strings.
>
> The compiler preprocessor was patched in Erlang/OTP 26.1 to warn about 3 or more sequential double quote characters. If the compiler should emit such a warning, please change all such double quote character sequences to have a whitespace after every second character, making the code more readable, and mean the same thing in all releases.

## Record

A record is a data structure for storing a fixed number of elements. It has named fields and is similar to a struct in C. However, a record is not a true data type. Instead, record expressions are translated to tuple expressions during compilation. Therefore, record expressions are not understood by the shell unless special actions are taken. For details, see the `m:shell` manual page in STDLIB).

*Examples:*

```text
-module(person).
-export([new/2]).

-record(person, {name, age}).

new(Name, Age) ->
    #person{name=Name, age=Age}.

1> person:new(ernie, 44).
{person,ernie,44}
```

Read more about records in [Records](ref_man_records.md). More examples can be found in [Programming Examples](`p:system:prog_ex_records.md`).

## Boolean

There is no Boolean data type in Erlang. Instead the atoms `true` and `false` are used to denote Boolean values.

*Examples:*

```text
1> 2 =< 3.
true
2> true or false.
true
```

## Escape Sequences

Within strings (`"`\-delimited) and quoted atoms, the following escape sequences are recognized:

| *Sequence* | *Description* |
|------------|---------------|
| `\b` | Backspace (ASCII code 8) |
| `\d` | Delete (ASCII code 127) |
| `\e` | Escape (ASCII code 27) |
| `\f` | Form Feed (ASCII code 12) |
| `\n` | Line Feed/Newline (ASCII code 10) |
| `\r` | Carriage Return (ASCII code 13) |
| `\s` | Space (ASCII code 32) |
| `\t` | (Horizontal) Tab (ASCII code 9) |
| `\v` | Vertical Tab (ASCII code 11) |
| `\`XYZ, `\`YZ, `\`Z | Character with octal representation XYZ, YZ or Z |
| `\xXY` | Character with hexadecimal representation XY |
| `\x{`X...`}` | Character with hexadecimal representation; X... is one or more hexadecimal characters |
| `\^a`...`\^z` `\^A`...`\^Z` | Control A to control Z |
| `\^@` | NUL (ASCII code 0) |
| `\^[` | Escape (ASCII code 27) |
| `\^\` | File Separator (ASCII code 28) |
| `\^]` | Group Separator (ASCII code 29) |
| `\^^` | Record Separator (ASCII code 30) |
| `\^_` | Unit Separator (ASCII code 31) |
| `\^?` | Delete (ASCII code 127) |
| `\'` | Single quote |
| `\"` | Double quote |
| `\\` | Backslash |


*Table: Recognized Escape Sequences*

> #### Change {: class=neutral }
> As of Erlang/OTP 26, the value of `$\^?` has been changed to be 127 (Delete), instead of 31. Previous releases would allow any character following `$\^`; as of Erlang/OTP 26, only the documented characters are allowed.

Within triple-quoted strings, escape sequences are not recognized. The only text that cannot be written in a triple-quoted string is three consecutive double quote characters at the beginning of a line (preceded only by whitespace). This limitation can be worked around by using more double quote characters for the string delimiters than in the string. Any number three or above is allowed.

> #### Change {: class=neutral }
> Triple quoted strings were introduced in Erlang/OTP 27.

## Type Conversions

There are a number of BIFs for type conversions.

*Examples:*

```text
1> atom_to_list(hello).
"hello"
2> list_to_atom("hello").
hello
3> binary_to_list(<<"hello">>).
"hello"
4> binary_to_list(<<104,101,108,108,111>>).
"hello"
5> list_to_binary("hello").
<<104,101,108,108,111>>
6> float_to_list(7.0).
"7.00000000000000000000e+00"
7> list_to_float("7.000e+00").
7.0
8> integer_to_list(77).
"77"
9> list_to_integer("77").
77
10> tuple_to_list({a,b,c}).
[a,b,c]
11> list_to_tuple([a,b,c]).
{a,b,c}
12> term_to_binary({a,b,c}).
<<131,104,3,100,0,1,97,100,0,1,98,100,0,1,99>>
13> binary_to_term(<<131,104,3,100,0,1,97,100,0,1,98,100,0,1,99>>).
{a,b,c}
14> binary_to_integer(<<"77">>).
77
15> integer_to_binary(77).
<<"77">>
16> float_to_binary(7.0).
<<"7.00000000000000000000e+00">>
17> binary_to_float(<<"7.000e+00">>).
7.0
```
