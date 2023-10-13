# Engine Load

[](){: id=engine_load }
This chapter describes the support for loading encryption engines in the crypto application.

## Background

OpenSSL exposes an Engine API, which makes it possible to plug in alternative implementations for some or all of the cryptographic operations implemented by OpenSSL. When configured appropriately, OpenSSL calls the engine's implementation of these operations instead of its own.

Typically, OpenSSL engines provide a hardware implementation of specific cryptographic operations. The hardware implementation usually offers improved performance over its software-based counterpart, which is known as cryptographic acceleration.

> #### Note {: class=info }
> The file name requirement on the engine dynamic library can differ between SSL versions.

## Use Cases

### Dynamically load an engine from default directory

If the engine is located in the OpenSSL/LibreSSL installation `engines` directory.

```text
1> {ok, Engine} = crypto:engine_load(<<"otp_test_engine">>, [], []).
 {ok, #Ref}
```

### Load an engine with the dynamic engine

Load an engine with the help of the dynamic engine by giving the path to the library.

```text
 2> {ok, Engine} = crypto:engine_load(<<"dynamic">>,
                                      [{<<"SO_PATH">>,
                                        <<"/some/path/otp_test_engine.so">>},
                                       {<<"ID">>, <<"MD5">>},
                                       <<"LOAD">>],
                                      []).
 {ok, #Ref}
```

### Load an engine and replace some methods

Load an engine with the help of the dynamic engine and just replace some engine methods.

```text
 3> {ok, Engine} = crypto:engine_load(<<"dynamic">>,
                                      [{<<"SO_PATH">>,
                                        <<"/some/path/otp_test_engine.so">>},
                                       {<<"ID">>, <<"MD5">>},
                                       <<"LOAD">>],
                                      []).
{ok, #Ref}
4> ok = crypto:engine_register(Engine, [engine_method_digests]).
ok
```

### Load with the ensure loaded function

This function makes sure the engine is loaded just once and the ID is added to the internal engine list of OpenSSL. The following calls to the function will check if the ID is loaded and then just get a new reference to the engine.

```text
 5> {ok, Engine} = crypto:ensure_engine_loaded(<<"MD5">>,
                                               <<"/some/path/otp_test_engine.so">>).
 {ok, #Ref}
```

To remove the tag from the OpenSSL engine list use `crypto:engine_remove/1`.

```text
 6> crypto:engine_remove(Engine).
 ok
```

To unload it use `crypto:engine_unload/1` which removes the references to the engine.

```text
 6> crypto:engine_unload(Engine).
 ok
```

### List all engines currently loaded

```text
 8> crypto:engine_list().
[<<"dynamic">>, <<"MD5">>]
```
