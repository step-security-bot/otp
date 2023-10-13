# erlc

Compiler

## Description

The `erlc` program provides a common way to run all compilers in the Erlang system. Depending on the extension of each input file, `erlc` invokes the appropriate compiler. Regardless of which compiler is used, the same flags are used to provide parameters, such as include paths and output directory.

The current working directory, `"."`, is not included in the code path when running the compiler. This is to avoid loading Beam files from the current working directory that could potentially be in conflict with the compiler or the Erlang/OTP system used by the compiler.

## erlc flags file1.ext file2.ext...

Compiles one or more files. The files must include the extension, for example, `.erl` for Erlang source code, or `.yrl` for Yecc source code. `Erlc` uses the extension to invoke the correct compiler.

## Generally Useful Flags

The following flags are supported:

* __`-I <Directory>`__ - Instructs the compiler to search for include files in the `Directory`. When encountering an `-include` or `-include_lib` directive, the compiler searches for header files in the following directories:

  * `"."`, the current working directory of the file server
  * The base name of the compiled file
  * The directories specified using option `-I`; the directory specified last is searched first

* __`-o <Directory>`__ - The directory where the compiler is to place the output files. Defaults to the current working directory.

* __`-D<Name>`__ - Defines a macro.

* __`-D<Name>=<Value>`__ - Defines a macro with the specified value. The value can be any Erlang term. Depending on the platform, the value may need to be quoted if the shell itself interprets certain characters. On Unix, terms containing tuples and lists must be quoted. Terms containing spaces must be quoted on all platforms.

* __`-WError`__ - Makes all warnings into errors.

* __`-W<Number>`__ - Sets warning level to `Number`. Defaults to `1`. To turn off warnings, use `-W0`.

* __`-W`__ - Same as `-W1`. Default.

* __`-v`__ - Enables verbose output.

* __`-b <Output_type>`__ - Specifies the type of output file. `Output_type` is the same as the file extension of the output file, but without the period. This option is ignored by compilers that have a single output format.

* __`-no-server`__ - Do not use the [compile server](erlc_cmd.md#compile_server).

* __`-server`__ - Use the [compile server](erlc_cmd.md#compile_server).

* __`-enable-feature <Feature>`{: id=enable-feature }__ - Enables the [feature](`p:system:features.md#features`) `feature` during compilation. The special feature `all` can be used to enable all features.

* __`-disable-feature <feature>`{: id=disable-feature }__ - Disables the [feature](`p:system:features.md#features`) `feature` during compilation. The special feature `all` can be used to disable all non permanent features.

* __`-list-features`__ - [](){: id=list-features }
  List short descriptions of the current configurable [features](`p:system:features.md#features`). Non configurable features, i.e., those with a status of `rejected` or `permanent` will not be shown.

* __`-describe-feature <feature>`__ - [](){: id=describe-feature }
  Show long description and history of [feature](`p:system:features.md#features`) `feature`.

* __`-M`__ - Produces a Makefile rule to track header dependencies. The rule is sent to `stdout`. No object file is produced.

* __`-MMD`__ - Generate dependencies as a side-effect. The object file will be produced as normal. This option overrides the option `-M`.

* __`-MF <Makefile>`__ - As option `-M`, except that the Makefile is written to `Makefile`. No object file is produced.

* __`-MD`__ - Same as `-M -MF <File>.Pbeam`.

* __`-MT <Target>`__ - In conjunction with option `-M` or `-MF`, changes the name of the rule emitted to `Target`.

* __`-MQ <Target>`__ - As option `-MT`, except that characters special to `make/1` are quoted.

* __`-MP`__ - In conjunction with option `-M` or `-MF`, adds a phony target for each dependency.

* __`-MG`__ - In conjunction with option `-M` or `-MF`, considers missing headers as generated files and adds them to the dependencies.

* __`--`__ - Signals that no more options will follow. The rest of the arguments is treated as filenames, even if they start with hyphens.

* __`+<Term>`__ - A flag starting with a plus (`+`) rather than a hyphen is converted to an Erlang term and passed unchanged to the compiler. For example, option `export_all` for the Erlang compiler can be specified as follows:

  ```text
  erlc +export_all file.erl
  ```

  Depending on the platform, the value may need to be quoted if the shell itself interprets certain characters. On Unix, terms containing tuples and lists must be quoted. Terms containing spaces must be quoted on all platforms.

## Special Flags

The following flags are useful in special situations, such as rebuilding the OTP system:

* __`-pa <Directory>`__ - Appends `Directory` to the front of the code path in the invoked Erlang emulator. This can be used to invoke another compiler than the default one.

* __`-pz <Directory>`__ - Appends `Directory` to the code path in the invoked Erlang emulator.

## Supported Compilers

The following compilers are supported:

* __`.erl`__ - Erlang source code. It generates a `.beam` file.

  Options `-P`, `-E`, and `-S` are equivalent to `+'P'`, `+'E'`, and `+'S'`, except that it is not necessary to include the single quotes to protect them from the shell.

  Supported options: `-I`, `-o`, `-D`, `-v`, `-W`, `-b`.

* __`.S`__ - Erlang assembler source code. It generates a `.beam` file.

  Supported options: same as for `.erl`.

* __`.core`__ - Erlang core source code. It generates a `.beam` file.

  Supported options: same as for `.erl`.

* __`.yrl`__ - Yecc source code. It generates an `.erl` file.

  Use option `-I` with the name of a file to use that file as a customized prologue file (option `includefile`).

  Supported options: `-o`, `-v`, `-I`, `-W`.

* __`.mib`__ - MIB for SNMP. It generates a `.bin` file.

  Supported options: `-I`, `-o`, `-W`.

* __`.bin`__ - A compiled MIB for SNMP. It generates a `.hrl` file.

  Supported options: `-o`, `-v`.

* __`.rel`__ - Script file. It generates a boot file.

  Use option `-I` to name directories to be searched for application files (equivalent to the `path` in the option list for `systools:make_script/2`).

  Supported option: `-o`.

* __`.asn1`__ - ASN1 file. It creates an `.erl`, `.hrl`, and `.asn1db` file from an `.asn1` file. Also compiles the `.erl` using the Erlang compiler unless option `+noobj` is specified.

  Supported options: `-I`, `-o`, `-b`, `-W`.

* __`.idl`__ - IC file. It runs the IDL compiler.

  Supported options: `-I`, `-o`.

[](){: id=compile_server }
## Compile Server

The compile server can be used to potentially speed up the build of multi-file projects by avoiding to start an Erlang system for each file to compile. Whether it will speed up the build depends on the nature of the project and the build machine.

By default, the compile server is not used. It can be enabled by giving `erlc` the option `-server` or by setting the environment variable `ERLC_USE_SERVER` to `yes` or `true`.

When the compile server is enabled, `erlc` will automatically use the server if it is started and start the server if has not already started. The server will terminate itself when it has been idle for some number of seconds.

`erlc` and the compile server communicate using the Erlang distribution. The compile server is started as a hidden node, with a name that includes the current user. Thus, each user on a computer has their own compile server.

Using the compile server does not always speed up the build, as the compile server sometimes must be restarted to ensure correctness. Here are some examples of situations that force a restart:

* `erlc` wants to use a different version of Erlang than the compile server is using.
* `erlc` wants to use different options for `erl` than the compile server was started with. (A change to code path using the option `-pa` could cause different parse transforms to be loaded. To be safe, the compile server will be restarted when any `erl` option is changed.)
* If the current working directory for `erlc` is different from the working directory active when the compile server was started, __and__ if the compile server has active jobs, it will be restarted as soon as those jobs have finished. (Build systems that build files randomly across multiple directories in parallel will probably not benefit from the compile server.)

[](){: id=environment_variables }
## Environment Variables

* __`ERLC_EMULATOR`__ - The command for starting the emulator. Defaults to `erl` in the same directory as the `erlc` program itself, or, if it does not exist, `erl` in any of the directories specified in environment variable `PATH`.

* __`ERLC_USE_SERVER`__ - Allowed values are `yes` or `true` to use the [compile server](erlc_cmd.md#compile_server), and `no` or `false` to not use the compile server. If other values are given, `erlc` will print a warning message and continue.

* __`ERLC_SERVER_ID`__ - Tells `erlc` to identify the [compile server](erlc_cmd.md#compile_server) by the given name, allowing a single user to run multiple unrelated builds in parallel without them affecting each other, which can be useful for shared build machines and the like. The name must be alpha­numeric, and it defaults to being empty.

## See Also

[`erl(1)`](erl_cmd.md), `m:compile`, `m:yecc`, `m:snmp`
