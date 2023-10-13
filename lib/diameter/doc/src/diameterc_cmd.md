# diameterc

diameterc \[<options>] <file>

## Description

The diameterc utility is used to compile a diameter [dictionary file](diameter_dict.md) into Erlang source. The resulting source implements the interface diameter required to encode and decode the dictionary's messages and AVPs.

The module `m:diameter_make` provides an alternate compilation interface.

## USAGE

* __diameterc \[<options>] <file>__ - Compile a single dictionary file to Erlang source. Valid options are as follows.

  * __\-i <dir>__ - Prepend the specified directory to the code path. Use to point at beam files compiled from inherited dictionaries, `[@inherits](diameter_dict.md#inherits)` in a dictionary file creating a beam dependency, not an erl/hrl dependency.

    Multiple `-i` options can be specified.

  * __\-o <dir>__ - Write generated source to the specified directory. Defaults to the current working directory.

  * __\-E__  

  * __\-H__ - Suppress erl and hrl generation, respectively.

  * __\--name <name>__  

  * __\--prefix <prefix>__ - Transform the input dictionary before compilation, setting `[@name](diameter_dict.md#name)` or `[@prefix](diameter_dict.md#prefix)` to the specified string.

  * __\--inherits <arg>__ - Transform the input dictionary before compilation, appending `[@inherits](diameter_dict.md#inherits)` of the specified string.

    Two forms of `--inherits` have special meaning:

    ```text
    --inherits -
    --inherits Prev/Mod
    ```

    The first has the effect of clearing any previous inherits, the second of replacing a previous inherits of `Prev` to one of `Mod`. This allows the semantics of the input dictionary to be changed without modifying the file itself.

    Multiple `--inherits` options can be specified.

  

## EXIT STATUS

Returns 0 on success, non-zero on failure.

## SEE ALSO

`m:diameter_make`, [diameter_dict(4)](diameter_dict.md)
