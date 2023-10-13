# typer

Typer, a Type annotator for ERlang programs.

## Description

TypEr shows type information for Erlang modules to the user. Additionally, it can annotate the code of files with such type information.

[](){: id=command_line }
## Using TypEr from the Command Line

TypEr is used from the command-line. This section provides a brief description of the options. The same information can be obtained by writing the following in a shell:

```text
typer --help
```

*Usage:*

```text
typer [--help] [--version] [--plt PLT] [--edoc]
      [--show | --show-exported | --annotate | --annotate-inc-files | --annotate-in-place]
      [-Ddefine]* [-I include_dir]* [-pa dir]* [-pz dir]*
      [-T application]* file* [-r directory*]
```

> #### Note {: class=info }
> \* denotes that multiple occurrences of the option are possible.

*Options:*

* __`-r`__ - Search directories recursively for .erl files below them. If a list of files is given, this must be after them.

* __`--show`__ - Print type specifications for all functions on stdout. (This is the default behaviour; this option is not really needed.)

* __`--show-exported` (or `show_exported`)__ - Same as `--show`, but print specifications for exported functions only. Specs are displayed sorted alphabetically on the function's name.

* __`--annotate`__ - Annotate the specified files with type specifications.

* __`--annotate-inc-files`__ - Same as `--annotate` but annotates all `-include()` files as well as all .erl files. (Use this option with caution - it has not been tested much).

* __`--annotate-in-place`__ - Annotate directly on the source code files, instead of dumping the annotated files in a different directory (use this option with caution - has not been tested much)

* __`--edoc`__ - Print type information as Edoc `@spec` comments, not as type specs.

* __`--plt`__ - Use the specified dialyzer PLT file rather than the default one.

* __`-T file*`__ - The specified file(s) already contain type specifications and these are to be trusted in order to print specs for the rest of the files. (Multiple files or dirs, separated by spaces, can be specified.)

* __`-Dname` (or `-Dname=value`)__ - Pass the defined name(s) to TypEr. (**)

* __`-I`__ - Pass the include_dir to TypEr. (**)

* __`-pa dir`__ - Include `dir` in the path for Erlang. This is useful when analyzing files that have `-include_lib()` directives or use parse transforms.

* __`-pz dir`__ - Include `dir` in the path for Erlang. This is useful when analyzing files that have `-include_lib()` directives or use parse transforms.

* __`--version` (or `-v`)__ - Print the TypEr version and some more information and exit.

> #### Note {: class=info }
> \** options `-D` and `-I` work both from the command line and in the TypEr GUI; the syntax of defines and includes is the same as that used by [erlc(1)](`p:erts:erlc_cmd.md`).
