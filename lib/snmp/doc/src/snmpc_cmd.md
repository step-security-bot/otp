# snmpc(command)

SNMP MIB compiler frontend

## Description

The `snmpc` program provides a way to run the SNMP MIB compiler of the Erlang system.

## snmpc \[options] file.mib | file.bin

`snmpc` compile a SNMP MIB file, see [compile/1,2](`m:snmpc#compile`) for more info.

It can also be used to generate a header file (.hrl) with definitions of Erlang constants for the objects in the MIB, see [mib_to_hrl/1](`m:snmpc#mib_to_hrl`).

[](){: id=options }
## Compiler options

The following options are supported (note that most of these relate to the compilation of the MIB file):

[](){: id=option_help }
* __\--help__ - Prints help info.

  [](){: id=option_version }

* __\--version__ - Prints application and mib format version.

  [](){: id=option_verbosity }

* __\--verbosity *verbosity*__ - Print debug info.

  `verbosity` = `trace` | `debug` | `log` | `info` | `silence`

  Defaults to `silence`.

  [](){: id=option_w }
  [](){: id=option_warnings }

* __\--warnings | --W__ - Print warning messages.

  [](){: id=option_wae }
  [](){: id=option_werror }

* __\--wae | --Werror__ - Warnings as errors. Indicates that warnings shall be treated as errors.

  [](){: id=option_odir }

* __\--o *directory*__ - The directory where the compiler should place the output files. If not specified, output files will be placed in the current working directory.

  [](){: id=option_idir }

* __\--i *Directory*__ - Specifies the path to search for imported (compiled) MIB files. By default, the current working directory is always included.

  This option can be present several times, each time specifying *one* path.

  [](){: id=option_ildir }

* __\--il *Directory*__ - This option (include_lib), specifies a list of directories to search for imported MIBs. It assumes that the first element in the directory name corresponds to an OTP application. The compiler will find the current installed version. For example, the value \["snmp/mibs/"] will be replaced by \["snmp-3.1.1/mibs/"] (or what the current version may be in the system). The current directory and the "snmp-home"/priv/mibs/ are always listed last in the include path.

  [](){: id=option_sgc }

* __\--sgc__ - This option (skip group check), if present, disables the group check of the mib compiler. That is, should the OBJECT-GROUP and the NOTIFICATION-GROUP macro(s) be checked for correctness or not.

  [](){: id=option_dep }

* __\--dep__ - Keep deprecated definition(s). If not specified the compiler will ignore deprecated definitions.

  [](){: id=option_desc }

* __\--desc__ - The DESCRIPTION field will be included.

  [](){: id=option_ref }

* __\--ref__ - The REFERENCE field will be included.

  [](){: id=option_imp }

* __\--imp__ - The IMPORTS field will be included.

  [](){: id=option_mi }

* __\--mi__ - The MODULE-IDENTITY field will be included.

  [](){: id=option_mc }

* __\--mc__ - The MODULE-COMPLIANCE field will be included.

  [](){: id=option_ac }

* __\--ac__ - The AGENT-CAPABILITIES field will be included.

  [](){: id=option_mod }

* __\--mod *module*__ - The module which implements all the instrumentation functions.

  The name of all instrumentation functions must be the same as the corresponding managed object it implements.

  [](){: id=option_nd }

* __\--nd__ - The default instrumentation functions will *not* be used if a managed object have no instrumentation function. Instead this will be reported as an error, and the compilation aborts.

  [](){: id=option_rrnac }

* __\--rrnac__ - This option, if present, specifies that the row name assign check shall not be done strictly according to the SMI (which allows only the value 1).

  With this option, all values greater than zero is allowed (>= 1). This means that the error will be converted to a warning.

  By default it is not included, but if this option is present it will be.

  [](){: id=see_also }

## SEE ALSO

[erlc(1)](`p:erts:erlc_cmd.md`), `m:compile`, `m:snmpc`
