# STDLIB

The STDLIB application.

## Description

The STDLIB application is mandatory in the sense that the minimal system based on Erlang/OTP consists of Kernel and STDLIB. The STDLIB application contains no services.

## Configuration

The following configuration parameters are defined for the STDLIB application. For more information about configuration parameters, see the [`app(4)`](`p:kernel:app.md`) module in Kernel.

* __`shell_esc = icl | abort`{: id=shell_esc }__ - Can be used to change the behavior of the Erlang shell when *^G* is pressed.

* __`restricted_shell = module()`{: id=restricted_shell }__ - Can be used to run the Erlang shell in restricted mode.

* __`shell_catch_exception = boolean()`{: id=shell_catch_exception }__ - Can be used to set the exception handling of the evaluator process of Erlang shell.

* __`shell_expand_location = above | below`{: id=shell_expand_location }__ - Sets where the tab expansion text should appear in the shell. The default is `below`.

* __`shell_history_length = integer() >= 0`{: id=shell_history_length }__ - Can be used to determine how many commands are saved by the Erlang shell. See `m:edlin` for more.

* __`shell_keymap = #{}`{: id=shell_keymap }__ - Can be used to override the default keymap configuration for the shell.

* __`shell_prompt_func = {Mod, Func} | default`{: id=shell_prompt_func }__ - where

  * `Mod = atom()`
  * `Func = atom()`

  Can be used to set a customized Erlang shell prompt function.

* __`shell_saved_results = integer() >= 0`{: id=shell_saved_results }__ - Can be used to determine how many results are saved by the Erlang shell.

* __`shell_session_slogan = string() | fun() -> string())`{: id=shell_session_slogan }__ - The slogan printed when starting an Erlang shell. Example:

  ```erlang
  $ erl -stdlib shell_session_slogan '"Test slogan"'
  Erlang/OTP 26 [DEVELOPMENT] [erts-13.0.2] [source] [64-bit] [smp:8:8] [ds:8:8:10] [async-threads:1] [jit:ns]
  
  Test slogan
  1>
  ```

* __`shell_slogan = string() | fun(() -> string())`{: id=shell_slogan }__ - The slogan printed when starting the Erlang shell subsystem. Example:

  ```erlang
  $ erl -stdlib shell_slogan '"Test slogan"'
  Test slogan
  Eshell V13.0.2  (abort with ^G)
  1>
  ```

  The default is the return value of [`erlang:system_info(system_version)`](`m:erlang#system_info_system_version`).

* __`shell_strings = boolean()`{: id=shell_strings }__ - Can be used to determine how the Erlang shell outputs lists of integers.

## See Also

[`app(4)`](`p:kernel:app.md`), `m:application`, `m:shell`
