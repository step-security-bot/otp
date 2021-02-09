# Modernize configurations

## To be implemented

### Implement -configfd
* `-configfd` that reads config from an FD (would normally be stdin), but can be anything else.
* init:restart/0 needs to work.
* the `HEART_COMMAND` will have to take care of when the VM crashes
* What do we do on release upgrades that require a restart? Do we
  pipe the previous config into the new release, or is this the job of heart?
  * Heart will have to manage this. If `-configfd` is used then the called
    [start_prg](https://erlang.org/doc/man/sasl_app.html#configuration)
    has to be able to handle that.

### Allow other configuration formats

* The ending of the config file. i.e. `-config sys.toml` will call the `toml_config_parser` module.
* `-config_parser toml mytoml_config_parser` can be used to set a custom config parser.
* Release upgrades should be possible to be done with a `sys.toml`, which means that release_handler
  should check the extension.
  * You cannot change the format of the config in a running system.
  * `release_handler:new_emulator_make_hybrid_config` will create a config in
    `.config` format which is based on the what you get from `sys.toml`.
* The parsing of external formats needs to be done before any application is started.
  This means that at the earliest it can be done when the AC starts, at the latest when
  `{progress,application_loaded}` happens in the boot script.
  * The config needs to be parsed that early because we need to run the `check_conf/1`
    callback before the application is started.
  * This means that if using an Elixir parser, the code will be loaded, but no processes
    will be started.

```
-type anno_config_key() :: {atom(),erl_anno:anno()}.
-type anno_config_value() :: {term(),erl_anno:anno()}.
-type anno_config() :: #{ anno_config_key() => anno_config_value() | anno_config() } |
                         [{ anno_config_key(), anno_config_value() | anno_config() }].
-type anno_app_config() :: {AppName :: anno_config_key(),
                            Config :: anno_config()}.

-spec parse(Data :: unicode:chardata(), Anno :: erl_anno:anno()) ->
          {ok, AppConfig :: [anno_app_config()], MoreFiles :: [file:name()]} |
          {error, Where :: erl_anno:anno(), Reason :: unicode:chardata()}.
```

Problems:
  * What if we want to write a config parser in elixir? POSTPONE

### Applications should configure the merge strategy

* Options: flat vs deep
  * Flat is the backwards compatible way
  * Deep does a tree merge of values.
    * Need to figure out a way to delete values. Overwrite with `undefined`?
  * Does this config need to be on a per key level? i.e.
    `{env_config,[#{ replace => [AllLegacyKeys] }]}`
    The problem here is `logger` in kernel. It is a map, where we want legacy
    config to work (though give a warning). But we want to re-use the same key
    in the new config scheme where we want the deepmerge strategy....
    NOTE: logger in kernel works as it should, I remembered incorrectly.
* If an application wants to work with both old and new configuration it should
  use the legacy merge way.
* If kernel does a live upgrade from OTP-23 to OTP-24, then the old merge will
  be used, as no other method is known to OTP-23.

* We cannot let the application decide how to merge as that will not work
  in emulator upgrade scenarios. In emulator upgrade scenarios it is the
  old emulator that needs to do the merge (for erts, stdlib, kernel, sasl).

* Add API `application:get_env(App,[erts,schedulers,online])` and
  `application:set_env(App,[erts,schedulers,online],Value)`.

### Allow application to check the configuration

* Callback that validates the config of application
* Failure to validate should stop start of application
  * Add option to warn or ignore such miss-configurations
* The configuration check should be done when loading the
  application.
  * Except for erts, stdlib and kernel, which should be done
    after each of the previous have been loaded.

```
-type config_key() :: atom().
-type config() :: #{ config_key() => config() | term() }.

-spec check_env(Env) ->
          ok | {error, [Location]} when
      Env :: config(),
      Reason :: unicode:chardata(),
      Path :: [config_key()],
      Location :: {invalid_value | invalid_key, Reason, Path}.
```

### Migrate all Erlang/OTP configuration to use new config system

* This includes all command line and environment variable options.
* Create an erts application used to configure erts
  * Move erts source code to lib/erts and update preloaded from there
* erts config needs special handling...
  * needs to be restarted with new parameters for those that are read-only
  * For windows we will build a special run_erl that first starts an emulator
    which can then start a new emulator if needed.
    * The new emulator started could just be the original process.

### Support read-only configuration parameters

* `application:set_env(Key,Value,[{mode,read_only}])`.
* This is configured on a per application basis.
  * Or possibly on a per primary key basis.

### Support testing of config

* erl -configcheck
  * Do a dry-run that calls check_env of all applications in .boot file.

## To maybe be implemented

### Allow multiple -config for release upgrades

* The main problem here is that it should probably be possible to add and remove
  `-config` statements during upgrade/downgrade which would mean that we have to
  parse ERL_FLAGS and any `vm.args` files in order to do that and that may be difficult...

### Implement -type spec style verification of config data

https://gist.github.com/garazdawi/5e80f16cc20de20550c1d6b958a12017

## Requirements

* Both per application and per system configurations
** Think of system as a product shipped to customer
* All applications (including erts) should be from the same source
* Testable
** Should be possible by tools to do verification of configuration before start
* Pluggable format
** Allow toml, yaml and etf
* Callbacks on change
* Automatic documentation in edoc and erl_docgen
* Uniform naming scheme

## Open questions

* Do we want runnable configurations?
* What do we do with erts config?

## Things to looks at

* cuttlefish
* elixir config
* init.d vs system.d vs sysctl

## Ideas

* Include description of application parameters in .app file
** Generate a .erl file from this that is the backend for the config
*** In the backend we can either use ets or persistent_term
*** In the backend we can install change hooks and verification
* Should we create an erts application that is also started?
* `will set` vs `did set` callback
* Autogenerated documentation from .app files
* When booting, can we load config early and if it changes something
  that requires a restart we restart the entire emulator with an `exec`?
* Can we remove net load from init?

# TOML Example

## TOML
```
## We can configure erts
[erts.alloc.util]
allocation_strategy = "bestfit"

[erts.alloc.binary]
enabled = false

## We can use both long and short names
[erts.alloc.E]
atags = true

## We can configure the kernel logger
[kernel.logger]
level = "all"

## And add handlers
[[kernel.logger.handler]]
id="file"
module="logger_std_h"
config.file = "log/erlang.log"
config.max_no_files = 5
config.max_no_bytes = 4096

[[kernel.logger.handler.filters]]
id = "remote_gl"
fun = "logger_filters:remote_gl/2"
arg = "stop"

[[kernel.logger.handler]]
id="debug"
module="logger_std_h"
level="debug"
config.file = "log/debug.log"
config.max_no_files = 5
config.max_no_bytes = 4096

```

## Erlang

```
{erts,[{alloc,#{ util => #{ allocation_strategy => "bestfit" },
                  binary => #{ enabled => false },
                  ets => #{ allocator\_tags => true } }}]}.
{kernel,[{logger,#{ level => all,
                    handler => [#{ id => "file", module => "logger_std_h",
                                   config => #{ file => "log/erlang.log",
                                                max_no_files => 5,
                                                max_no_bytes => 4096 },
                                   filters => [#{ id = "remote_gl",
                                                  fun = "logger_filters:remote_gl/2",
                                                  arg = "stop" }] },
                                #{ id => debug,  module => "logger_std_h",
                                   level => debug,
                                   config => #{ file => "log/debug.log",
                                                max_no_files => 5,
                                                max_no_bytes => 4096 }}]
                   }}]}.
```
