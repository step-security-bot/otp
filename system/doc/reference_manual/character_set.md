# Character Set and Source File Encoding

## Character Set

The syntax of Erlang tokens allow the use of the full ISO-8859-1 (Latin-1) character set. This is noticeable in the following ways:

* All the Latin-1 printable characters can be used and are shown without the escape backslash convention.
* Unquoted atoms and variables can use all Latin-1 letters.

| *Octal* | *Decimal* |   | *Class* |
|---------|-----------|---|---------|
| 200 - 237 | 128 - 159 |   | Control characters |
| 240 - 277 | 160 - 191 | \- ¿ | Punctuation characters |
| 300 - 326 | 192 - 214 | À - Ö | Uppercase letters |
| 327 | 215 | × | Punctuation character |
| 330 - 336 | 216 - 222 | Ø - Þ | Uppercase letters |
| 337 - 366 | 223 - 246 | ß - ö | Lowercase letters |
| 367 | 247 | ÷ | Punctuation character |
| 370 - 377 | 248 - 255 | ø - ÿ | Lowercase letters |


*Table: Character Classes*

The following tokens are allowed to also use Unicode characters outside of the Latin-1 range:

* String literals. Example: `"√π"`
* Character literals. Example: `$∑`
* Comments in code.
* Quoted atoms. Example: `'μs'`
* Function names. Example: `'s_to_μs'(S) -> S * 1_000_000.`

Atoms used as module names, application names, and node names are restricted to the Latin-1 range.

> #### Change {: class=neutral }
> Support for Unicode in string literals, character literals, and comments was introduced in Erlang/OTP R16B. Support for Unicode in atom and function names was introduced in Erlang/OTP 20.

## Source File Encoding

[](){: id=encoding }
The Erlang source file `encoding` is selected by a comment in one of the first two lines of the source file. The first string that matches the regular expression `coding\s*[:=]\s*([-a-zA-Z0-9])+` selects the encoding. If the matching string is an invalid encoding, it is ignored. The valid encodings are `Latin-1` and `UTF-8`, where the case of the characters can be chosen freely.

The default Erlang source file encoding if no valid `coding` comment is present is UTF-8.

Two examples, both selecting Latin-1 as the source file encoding:

```text
%% For this file we have chosen encoding = Latin-1
```

```text
%% -*- coding: latin-1 -*-
```

> #### Change {: class=neutral }
> The default encoding for Erlang source files was changed from Latin-1 to UTF-8 in Erlang/OTP 17.0.
