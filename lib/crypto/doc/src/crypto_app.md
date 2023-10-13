# crypto

The Crypto Application

## Description

The purpose of the Crypto application is to provide an Erlang API to cryptographic functions, see `m:crypto`. Note that the API is on a fairly low level and there are some corresponding API functions available in `m:public_key`, on a higher abstraction level, that uses the crypto application in its implementation.

## DEPENDENCIES

The current crypto implementation uses nifs to interface OpenSSLs crypto library and may work with limited functionality with as old versions as *OpenSSL* 0\.9.8c. FIPS mode support requires at least version 1.0.1 and a FIPS capable OpenSSL installation. We recommend using a version that is officially supported by the OpenSSL project. API compatible backends like LibreSSL should also work.

The crypto app is tested daily with at least one version of each of the OpenSSL 1.0.1, 1.0.2, 1.1.0, 1.1.1 and 3.0. FIPS mode is also tested for 1.0.1, 1.0.2 and 3.0.

Using OpenSSL 3.0 with Engines is not yet supported by the OTP/crypto app.

Source releases of OpenSSL can be downloaded from the [OpenSSL](http://www.openssl.org) project home page, or mirror sites listed there.

## CONFIGURATION

The following configuration parameters are defined for the crypto application. See [`app(3)`](`p:kernel:app.md`) for more information about configuration parameters.

* __`fips_mode = boolean()`__ - Specifies whether to run crypto in FIPS mode. This setting will take effect when the nif module is loaded. If FIPS mode is requested but not available at run time the nif module and thus the crypto module will fail to load. This mechanism prevents the accidental use of non-validated algorithms.

* __`rand_cache_size = integer()`__ - Sets the cache size in bytes to use by [`crypto:rand_seed_alg(crypto_cache)` ](`crypto:rand_seed_alg/1`)and [`crypto:rand_seed_alg_s(crypto_cache)` ](`crypto:rand_seed_alg_s/1`). This parameter is read when a seed function is called, and then kept in generators state object. It has a rather small default value that causes reads of strong random bytes about once per hundred calls for a random value. The set value is rounded up to an integral number of words of the size these seed functions use.

## SEE ALSO

application(3)
