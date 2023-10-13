# Features

[](){: id=features }
Introduced in OTP 25, Erlang has the concept of selectable features. A feature can change, add or remove behaviour of the language and/or runtime system. Examples can include

* Adding new syntactical constructs to the language
* Change the semantics of an existing construct
* Change the behaviour of some runtime aspect

A feature will start out with a status of experimental part of OTP, making it possible to try out for users and give feedback. The possibility to try out features is enabled by options to the compiler, directives in a module and options to the runtime system. Even when a feature is not experimental it will still be possible to enable or disable it. This makes it possible to adapt a code base at a suitable pace instead of being forced when changing to a new release.

The status of a feature will eventually end up as being either a permanent part of OTP or rejected, being removed and no longer selectable.

## Life cycle of features

A feature is in one of four possible states:

* __Experimental__ - The initial state, is meant for trying out and collecting feedback. The feature can be enabled but is disabled by default.

* __Approved__ - The feature has been finalised and is now part of OTP. By default it is enabled, but can be disabled.

* __Permanent__ - The feature is now a permanent part of OTP. It can no longer be disabled.

* __Rejected__ - The feature never reached the approved state and will not be part of OTP. It cannot be enabled.

After leaving the experimental state, a feature can enter any of the other three states, and if the next state is approved, the feature will eventually end up in the permanent state. A feature can change state only in connection with a release.

A feature may be in the approved state for several releases.

| State | Default | Configurable | Available |
|-------|---------|--------------|-----------|
| Experimental | disabled | yes | yes |
| Approved | enabled | yes | yes |
| Permanent | enabled | no | yes |
| Rejected | disabled | no | no |


*Table: Feature States*

* Being configurable means the possibility to enable or disable the feature by means of compiler options and directives in the file being compiled.
* Being available can be seen using the `FEATURE_AVAILABLE` macro.

## Enabling and Disabling Features

To use a feature that is in the experimental state, it has to be enabled during compilation. This can be done in a number of different ways:

* __Options to `erlc`__ - Options [`-enable-feature`](`p:erts:erlc_cmd.md#enable-feature`) and [`-disable-feature`](`p:erts:erlc_cmd.md#disable-feature`) can be used to enable or disable individal features.

* __Compiler options__ - The compiler option [`{feature, <feature>, enable|disable}`](`m:compile#feature-option`) can be used either as a `+<term>` option to `erlc` or in the options argument to functions in the `compile` module.

* __The feature directive__ - Inside a prefix of a module, one can use a [`-feature(<feature>, enable|disable)`](macros.md#feature-directive) directive. This is the preferred method of enabling and disabling features.

> #### Change {: class=neutral }
> In Erlang/OTP 25, in order to load a module with a feature enabled, it was necessary to also enable the feature in the runtime. This was done using option [`-enable-feature`](`p:erts:erl_cmd.md#enable-feature`) to `erl`. This requirement was removed in Erlang/OTP 26.

## Preprocessor Additions

To allow for conditional compilation during transitioning of a code base and/or trying out experimental features [feature](`p:system:macros.md#predefined-macros`) `predefined macros` `?FEATURE_AVAILABLE(Feature)` and `?FEATURE_ENABLED(Feature)` are available.

## Information about Existing Features

The module `erl_features` `m:erl_features` exports a number of functions that can be used to obtain information about current features as well as the features used when compiling a module.

One can also use the `erlc` options [`-list-features`](`p:erts:erlc_cmd.md#list-features`) and [`-describe-feature <feature>`](`p:erts:erlc_cmd.md#describe-feature`) to get information about existing features.

Additionally, there is the compiler option [`warn_keywords`](`m:compile#warn-keywords`) that can be used to find atoms in the code base that might collide with keywords in features not yet enabled.

## Existing Features

The following configurable features exist:

* __`maybe_expr` (experimental)__ - Implementation of the [`maybe`](expressions.md#maybe) expression proposed in [EEP 49](https://www.erlang.org/eeps/eep-0049).
