# SNMP

The SNMP Application

## Description

This chapter describes the `snmp` application in OTP. The SNMP application provides the following services:

* a multilingual extensible SNMP agent
* a SNMP manager
* a MIB compiler

[](){: id=configuration_params }
## Configuration

The following configuration parameters are defined for the SNMP application. Refer to application(3) for more information about configuration parameters.

The snmp part of the config file specifying the configuration parameters is basically the following tuple:

```text
      {snmp, snmp_components_config()}
```

A minimal config file for starting a node with both a manager and an agent:

```text
      [{snmp, 
        [{agent, [{db_dir, "/tmp/snmp/agent/db"},
                  {config, [{dir, "/tmp/snmp/agent/conf"}]}]},
         {manager, [{config, [{dir, "/tmp/snmp/manager/conf"},
                              {db_dir, "/tmp/snmp/manager/db"}]}]}]}
        ]
       }
      ].
```

Each snmp component has its own set of configuration parameters, even though some of the types are common to both components.

```text
      snmp_components_config() -> [snmp_component_config()]
      snmp_component_config() -> {agent, agent_options()} | {manager, manager_options()}
      agent_options() = [agent_option()]
      agent_option() = {restart_type,     restart_type()}     | 
                       {agent_type,       agent_type()}       |  
                       {agent_verbosity,  verbosity()}        |  
                       {discovery,        agent_discovery()}  |  
                       {versions,         versions()}         |  
                       {gb_max_vbs,       gb_max_vbs()}       |  
                       {priority,         priority()}         |  
                       {multi_threaded,   multi_threaded()}   |  
                       {db_dir,           db_dir()}           |  
                       {db_init_error,    db_init_error()}    |  
                       {local_db,         local_db()}         |  
                       {net_if,           agent_net_if()}     |  
                       {mibs,             mibs()}             |  
                       {mib_storage,      mib_storage()}      |  
                       {mib_server,       mib_server()}       |  
                       {audit_trail_log,  audit_trail_log()}  |  
                       {error_report_mod, error_report_mod()} |  
                       {note_store,       note_store()}       |  
                       {symbolic_store,   symbolic_store()}   |  
                       {target_cache,     target_cache()}     |  
                       {config,           agent_config()}
      manager_options() = [manager_option()]
      manager_option() = {restart_type,             restart_type()}    | 
                         {net_if,                   manager_net_if()}  |  
                         {server,                   server()}          | 
                         {note_store,               note_store()}      | 
                         {config,                   manager_config()}  |  
                         {inform_request_behaviour, manager_irb()}     | 
                         {mibs,                     manager_mibs()}    | 
                         {priority,                 priority()}        |  
                         {audit_trail_log,          audit_trail_log()} | 
                         {versions,                 versions()}        | 
                         {def_user_mod,             def_user_module()  | 
                         {def_user_data,            def_user_data()}
```

[](){: id=agent_opts_and_types }
Agent specific config options and types:

* __`agent_type() = master | sub <optional>`{: id=agent_type }__ - If `master`, one master agent is started. Otherwise, no agents are started.

  Default is `master`.

* __`agent_discovery() = [agent_discovery_opt()] <optional>`{: id=agent_disco }__ - `agent_discovery_opt() = {terminating, agent_terminating_discovery_opts()} | {originating, agent_originating_discovery_opts()}`

  The `terminating` options effects discovery initiated by a manager.

  The `originating` options effects discovery initiated by this agent.

  For defaults see the options in `agent_discovery_opt()`.

* __`agent_terminating_discovery_opts() = [agent_terminating_discovery_opt()] <optional>`{: id=agent_term_disco_opts }__ - `agent_terminating_discovery_opt() = {enable, boolean()} | {stage2, discovery | plain} | {trigger_username, string()}`

  These are options effecting discovery `terminating` in this agent (i.e. initiated by a manager).

  The default values for the `terminating` discovery options are:

  * enable: `true`
  * stage2: `discovery`
  * trigger_username: `""`

* __`agent_originating_discovery_opts() = [agent_originating_discovery_opt()] <optional>`{: id=agent_orig_disco_opts }__ - `agent_originating_discovery_opt() = {enable, boolean()}`

  These are options effecting discovery `originating` in this agent.

  The default values for the `originating` discovery options are:

  * enable: `true`

* __`multi_threaded() = bool() | extended <optional>`{: id=agent_mt }__ - If `true` (or `extended`), the agent is multi-threaded, with one thread for each get request.

  The value `extended` means that a special 'process' is also created intended to handle *all* notifications.

  * `true` \- One worker dedicated to 'set-requests' and one (main) worker for all other requests ('get-request' and notifications).

    If the 'main' worker is busy, a temporary process is spawned to handle that job ('get-request' or notification).
  * `extended` \- One worker dedicated to 'set-requests', one worker dedicated to notifications and one (main) worker for all 'get-requests'.

    If the 'main' worker is busy, a temporary process is spawned to handle that 'get-request'.

  > #### Note {: class=info }
  > Even with multi-threaded set to `extended` there is still a risk for 'reorder' when sending inform-requsts, which require a response (and may therefore require resending).
  >
  > Also, there is of course no way to guarantee order once the package is on the network.

  Default is `false`.

* __`db_dir() = string() <mandatory>`{: id=agent_data_dir }__ - Defines where the SNMP agent internal db files are stored.

* __`gb_max_vbs() = pos_integer() | infinity <optional>`{: id=agent_gb_max_vbs }__ - Defines the maximum number of varbinds allowed in a Get-BULK response.

  Default is `1000`.

* __`local_db() = [local_db_opt()] <optional>`{: id=agent_local_db }__ - `local_db_opt() = {repair, agent_repair()} | {auto_save, agent_auto_save()} | {verbosity, verbosity()}`

  Defines options specific for the SNMP agent local database.

  For defaults see the options in `local_db_opt()`.

* __`agent_repair() = false | true | force <optional>`{: id=agent_ldb_repair }__ - When starting snmpa_local_db it always tries to open an existing database. If `false`, and some errors occur, a new database is created instead. If `true`, an existing file will be repaired. If `force`, the table will be repaired even if it was properly closed.

  Default is `true`.

* __`agent_auto_save() = integer() | infinity <optional>`{: id=agent_ldb_auto_save }__ - The auto save interval. The table is flushed to disk whenever not accessed for this amount of time.

  Default is `5000`.

* __`agent_net_if() = [agent_net_if_opt()] <optional>`{: id=agent_net_if }__ - `agent_net_if_opt() = {module, agent_net_if_module()} | {verbosity, verbosity()} | {options, agent_net_if_options()}`

  Defines options specific for the SNMP agent network interface entity.

  For defaults see the options in `agent_net_if_opt()`.

* __`agent_net_if_module() = atom() <optional>`{: id=agent_ni_module }__ - Module which handles the network interface part for the SNMP agent. Must implement the `m:snmpa_network_interface` behaviour.

  Default is `snmpa_net_if`.

* __`agent_net_if_options() = [agent_net_if_option()] <optional>`{: id=agent_ni_opts }__ - `agent_net_if_option() = {bind_to, bind_to()} | {sndbuf, sndbuf()} | {recbuf, recbuf()} | {no_reuse, no_reuse()} | {req_limit, req_limit()} | {filter, agent_net_if_filter_options()} | {open_err_filters, agent_net_if_open_err_filters()} | {extra_sock_opts, extra_socket_options()} | {inet_backend, inet_backend()}`

  These options are actually specific to the used module. The ones shown here are applicable to the default `agent_net_if_module()`.

  > #### Note {: class=info }
  > If the user has configured transports *with* options then those will take precedence over these options. See [agent information](snmp_agent_config_files.md#agent_information) for more info.

  For defaults see the options in `agent_net_if_option()`.

* __`req_limit() = integer() | infinity <optional>`{: id=agent_ni_req_limit }__ - Max number of simultaneous requests handled by the agent.

  Default is `infinity`.

* __`agent_net_if_filter_options() = [agent_net_if_filter_option()] <optional>`{: id=agent_ni_filter_opts }__ - `agent_net_if_filter_option() = {module, agent_net_if_filter_module()}`

  These options are actually specific to the used module. The ones shown here are applicable to the default `agent_net_if_filter_module()`.

  For defaults see the options in `agent_net_if_filter_option()`.

* __`agent_net_if_filter_module() = atom() <optional>`{: id=agent_ni_filter_module }__ - Module which handles the network interface filter part for the SNMP agent. Must implement the `m:snmpa_network_interface_filter` behaviour.

  Default is `snmpa_net_if_filter`.

* __`agent_net_if_open_err_filters() = [agent_net_if_open_err_filter()] <optional>`{: id=agent_ni_open_err_filters }__ - `agent_net_if_open_err_filter() = atom()`

  During agent initiation, the transports UDP sockets are opened. If this operation fails, the net-if (and the agent) fails to start (crash). This (filter) list contains error (reasons) that will make net-if fail "nicely". This (filter) list, is supposed to contain errors that can be returned by [gen_udp:open/1,2](`gen_udp:open/1`). The effect is that any error returned by [gen_udp:open](`gen_udp:open/1`) which *are* in this list, will be considered "non-fatal" and will only result in an info message, rather than an error message. Net If, and the agent, will still crash, but will produce a less obnoxious message.

* __`agent_mibs() = [string()] <optional>`{: id=agent_mibs }__ - Specifies a list of MIBs (including path) that defines which MIBs are initially loaded into the SNMP master agent.

  Note that the following mibs will always be loaded:

  * version v1: `STANDARD-MIB`
  * version v2: `SNMPv2`
  * version v3: `SNMPv2`, `SNMP-FRAMEWORK-MIB` and `SNMP-MPD-MIB`

  Default is `[]`.

* __`mib_storage() = [mib_storage_opt()] <optional>`{: id=agent_mib_storage }__ - `mib_storage_opt() = {module, mib_storage_module()} | {options, mib_storage_options()}`

  This option specifies how basic mib data is stored. This option is used by two parts of the snmp agent: The mib-server and the symbolic-store.

  Default is `[{module, snmpa_mib_storage_ets}]`.

* __`mib_storage_module() = snmpa_mib_data_ets | snmpa_mib_data_dets | snmpa_mib_data_mnesia | module()`{: id=agent_mst_module }__ - Defines the mib storage module of the SNMP agent as defined by the `m:snmpa_mib_storage` behaviour.

  Several entities (`mib-server` via the its data module and the `symbolic-store`) of the snmp agent uses this for storage of miscellaneous mib related data retrieved while loading a mib.

  There are several implementations provided with the agent: `snmpa_mib_storage_ets`, `snmpa_mib_storage_dets` and `snmpa_mib_storage_mnesia`.

  Default module is `snmpa_mib_storage_ets`.

* __`mib_storage_options() = list() <optional>`{: id=agent_mst_options }__ - This is implementation depended. That is, it depends on the module. For each module a specific set of options are valid. For the module provided with the app, these options are supported:

  * `snmpa_mib_storage_ets`: `{dir, filename()} | {action, keep | clear}, {checksum, boolean()}`

    * `dir` \- If present, points to a directory where a file to which all data in the ets table is "synced".

      Also, when a table is opened this file is read, if it exists.

      By default, this will *not* be used.
    * `action` \- Specifies the behaviour when a non-empty file is found: Keep its content or clear it out.

      Default is `keep`.
    * `checksum` \- Defines if the file is checksummed or not.

      Default is `false`.
  * `snmpa_mib_storage_dets`: `{dir, filename()} | {action, keep | clear}, {auto_save, default | pos_integer()} | {repair, force | boolean()}`

    * `dir` \- This *mandatory* option points to a directory where to place the file of a dets table.
    * `action` \- Specifies the behaviour when a non-empty file is found: Keep its content or clear it out.

      Default is `keep`.
    * `auto_save` \- Defines the dets auto-save frequency.

      Default is `default`.
    * `repair` \- Defines the dets repair behaviour.

      Default is `false`.
  * `snmpa_mib_storage_mnesia`: `{action, keep | clear}, {nodes, [node()]}`

    * `action` \- Specifies the behaviour when a non-empty, already existing, table: Keep its content or clear it out.

      Default is `keep`.
    * `nodes` \- A list of node names (or an atom describing a list of nodes) defining where to open the table. Its up to the user to ensure that mnesia is actually running on the specified nodes.

      The following distinct values are recognised:

      * `[]` \- Translated into a list of the own node: `[node()]`
      * `all` \- `erlang:nodes()`
      * `visible` \- `erlang:nodes(visible)`
      * `connected` \- `erlang:nodes(connected)`
      * `db_nodes` \- `mnesia:system_info(db_nodes)`

      Default is the result of the call: `erlang:nodes()`.

* __`mib_server() = [mib_server_opt()] <optional>`{: id=agent_mib_server }__ - `mib_server_opt() = {mibentry_override, mibentry_override()} | {trapentry_override, trapentry_override()} | {verbosity, verbosity()} | {cache, mibs_cache()} | {data_module, mib_server_data_module()}`

  Defines options specific for the SNMP agent mib server.

  For defaults see the options in `mib_server_opt()`.

* __`mibentry_override() = bool() <optional>`{: id=agent_ms_meo }__ - If this value is false, then when loading a mib each mib- entry is checked prior to installation of the mib. The purpose of the check is to prevent that the same symbolic mibentry name is used for different oid's.

  Default is `false`.

* __`trapentry_override() = bool() <optional>`{: id=agent_ms_teo }__ - If this value is false, then when loading a mib each trap is checked prior to installation of the mib. The purpose of the check is to prevent that the same symbolic trap name is used for different trap's.

  Default is `false`.

* __`mib_server_data_module() = snmpa_mib_data_tttn | module() <optional>`{: id=agent_ms_data_module }__ - Defines the backend data module of the SNMP agent mib-server as defined by the `m:snmpa_mib_data` behaviour.

  At present only the default module is provided with the agent, `snmpa_mib_data_tttn`.

  Default module is `snmpa_mib_data_tttn`.

* __`mibs_cache() = bool() | mibs_cache_opts() <optional>`{: id=agent_ms_cache }__ - Shall the agent utilize the mib server lookup cache or not.

  Default is `true` (in which case the `mibs_cache_opts()` default values apply).

* __`mibs_cache_opts() = [mibs_cache_opt()] <optional>`{: id=agent_ms_cache_opts }__ - `mibs_cache_opt() = {autogc, mibs_cache_autogc()} | {gclimit, mibs_cache_gclimit()} | {age, mibs_cache_age()}`

  Defines options specific for the SNMP agent mib server cache.

  For defaults see the options in `mibs_cache_opt()`.

* __`mibs_cache_autogc() = bool() <optional>`{: id=agent_ms_cache_autogc }__ - Defines if the mib server shall perform cache gc automatically or leave it to the user (see [gc_mibs_cache/0,1,2,3](`m:snmpa#gc_mibs_cache`)).

  Default is `true`.

* __`mibs_cache_age() = integer() > 0 <optional>`{: id=agent_ms_cache_age }__ - Defines how old the entries in the cache will be allowed to become before they are GC'ed (assuming GC is performed). Each entry in the cache is "touched" whenever it is accessed.

  The age is defined in milliseconds.

  Default is `10 timutes`.

* __`mibs_cache_gclimit() = infinity | integer() > 0 <optional>`{: id=agent_ms_cache_gclimit }__ - When performing a GC, this is the max number of cache entries that will be deleted from the cache.

  The reason why its possible to set a limit, is that if the cache is large, the GC can potentially take a long time, during which the agent is "busy". *But* on a heavily loaded system, we also risk not removing enough elements in the cache, instead causing it to grow over time. This is the reason the default value is `infinity`, which will ensure that *all* candidates are removed as soon as possible.

  Default is `infinity`.

* __`error_report_mod() = atom() <optional>`{: id=agent_error_report_mod }__ - Defines an error report module, implementing the `m:snmpa_error_report` behaviour. Two modules are provided with the toolkit: `snmpa_error_logger` and `snmpa_error_io`.

  Default is `snmpa_error_logger`.

* __`symbolic_store() = [symbolic_store_opt()]`{: id=agent_symbolic_store }__ - `symbolic_store_opt() = {verbosity, verbosity()}`

  Defines options specific for the SNMP agent symbolic store.

  For defaults see the options in `symbolic_store_opt()`.

* __`target_cache() = [target_cache_opt()]`{: id=agent_target_cache }__ - `target_cache_opt() = {verbosity, verbosity()}`

  Defines options specific for the SNMP agent target cache.

  For defaults see the options in `target_cache_opt()`.

* __`agent_config() = [agent_config_opt()] <mandatory>`{: id=agent_config }__ - `agent_config_opt() = {dir, agent_config_dir()} | {force_load, force_load()} | {verbosity, verbosity()}`

  Defines specific config related options for the SNMP agent.

  For defaults see the options in `agent_config_opt()`.

* __`agent_config_dir = dir() <mandatory>`{: id=agent_config_dir }__ - Defines where the SNMP agent configuration files are stored.

* __`force_load() = bool() <optional>`{: id=agent_force_load }__ - If `true` the configuration files are re-read during start-up, and the contents of the configuration database ignored. Thus, if `true`, changes to the configuration database are lost upon reboot of the agent.

  Default is `false`.

[](){: id=manager_opts_and_types }
Manager specific config options and types:

* __`server() = [server_opt()] <optional>`{: id=manager_server }__ - `server_opt() = {timeout, server_timeout()} | {verbosity, verbosity()} | {cbproxy, server_cbproxy()} | {netif_sup, server_nis()}`

  Specifies the options for the manager server process.

  Default is `silence`.

* __`server_timeout() = integer() <optional>`{: id=manager_server_timeout }__ - Asynchronous request cleanup time. For every requests, some info is stored internally, in order to be able to deliver the reply (when it arrives) to the proper destination. If the reply arrives, this info will be deleted. But if there is no reply (in time), the info has to be deleted after the *best before* time has been passed. This cleanup will be performed at regular intervals, defined by the `server_timeout()` time. The information will have an *best before* time, defined by the `Expire` time given when calling the request function (see [async_get](`m:snmpm#async_get2`), [async_get_next](`m:snmpm#async_get_next2`) and [async_set](`m:snmpm#async_set2`)).

  Time in milli-seconds.

  Default is `30000`.

* __`server_cbproxy() = temporary (default) | permanent <optional>`{: id=manager_server_cbproxy }__ - This option specifies how the server will handle callback calls.

  * __`temporary (default)`{: id=manager_server_cbproxy_temporary }__ - A temporary process will be created for each callback call.

  * __`permanent`{: id=manager_server_cbproxy_temporary }__ - With this the server will create a permanent (named) process that in effect serializes all callback calls.

  Default is `temporary`.

* __`server_nis() = none (default) | {PingTO, PongTO} <optional>`{: id=manager_server_nis }__ - This option specifies if the server should actively supervise the net-if process. Note that this will only work if the used net-if process actually supports the protocol. See `m:snmpm_network_interface` behaviour for more info.

  * __`none (default)`{: id=manager_server_nis_none }__ - No active supervision of the net-if process.

  * __`{PingTO :: pos_integer(), PongTO :: pos_integer()}`{: id=manager_server_nis_active }__ - The `PingTO` time specifies the between a successful ping (or start) and the time when a ping message is to be sent to the net-if process (basically the time between ping).

    The `PongTO` time specifies how long time the net-if process has to respond to a ping message, with a *pong* message. Its starts counting when the ping message has been sent.

    Both times are in milli seconds.

  Default is `none`.

* __`manager_config() = [manager_config_opt()] <mandatory>`{: id=manager_config }__ - `manager_config_opt() = {dir, manager_config_dir()} | {db_dir, manager_db_dir()} | {db_init_error, db_init_error()} | {repair, manager_repair()} | {auto_save, manager_auto_save()} | {verbosity, verbosity()}`

  Defines specific config related options for the SNMP manager.

  For defaults see the options in `manager_config_opt()`.

* __`manager_config_dir = dir() <mandatory>`{: id=manager_config_dir }__ - Defines where the SNMP manager configuration files are stored.

* __`manager_db_dir = dir() <mandatory>`{: id=manager_config_db_dir }__ - Defines where the SNMP manager store persistent data.

* __`manager_repair() = false | true | force <optional>`{: id=manager_config_repair }__ - Defines the repair option for the persistent database (if and how the table is repaired when opened).

  Default is `true`.

* __`manager_auto_save() = integer() | infinity <optional>`{: id=manager_config_auto_save }__ - The auto save interval. The table is flushed to disk whenever not accessed for this amount of time.

  Default is `5000`.

* __`manager_irb() = auto | user | {user, integer()} <optional>`{: id=manager_irb }__ - This option defines how the manager will handle the sending of response (acknowledgment) to received inform-requests.

  * `auto` \- The manager will autonomously send response (acknowledgment> to inform-request messages.
  * `{user, integer()}` \- The manager will send response (acknowledgment) to inform-request messages when the [handle_inform](`m:snmpm_user#handle_inform`) function completes. The integer is the time, in milli-seconds, that the manager will consider the stored inform-request info valid.
  * `user` \- Same as `{user, integer()}`, except that the default time, 15 seconds (15000), is used.

  See `m:snmpm_network_interface`, [handle_inform](`m:snmpm_user`) and [definition of the manager net if](snmp_manager_netif.md) for more info.

  Default is `auto`.

* __`manager_mibs() = [string()] <optional>`{: id=manager_mibs }__ - Specifies a list of MIBs (including path) and defines which MIBs are initially loaded into the SNMP manager.

  Default is `[]`.

* __`manager_net_if() = [manager_net_if_opt()] <optional>`{: id=manager_net_if }__ - `manager_net_if_opt() = {module, manager_net_if_module()} | {verbosity, verbosity()} | {options, manager_net_if_options()}`

  Defines options specific for the SNMP manager network interface entity.

  For defaults see the options in `manager_net_if_opt()`.

* __`manager_net_if_options() = [manager_net_if_option()] <optional>`{: id=manager_ni_opts }__ - `manager_net_if_option() = {bind_to, bind_to()} | {sndbuf, sndbuf()} | {recbuf, recbuf()} | {no_reuse, no_reuse()} | {filter, manager_net_if_filter_options()} | {extra_sock_opts, extra_socket_options()}} | {inet_backend, inet_backend()}`

  These options are actually specific to the used module. The ones shown here are applicable to the default `manager_net_if_module()`.

  For defaults see the options in `manager_net_if_option()`.

* __`manager_net_if_module() = atom() <optional>`{: id=manager_ni_module }__ - The module which handles the network interface part for the SNMP manager. It must implement the `m:snmpm_network_interface` behaviour.

  Default is `snmpm_net_if`.

* __`manager_net_if_filter_options() = [manager_net_if_filter_option()] <optional>`{: id=manager_ni_filter_opts }__ - `manager_net_if_filter_option() = {module, manager_net_if_filter_module()}`

  These options are actually specific to the used module. The ones shown here are applicable to the default `manager_net_if_filter_module()`.

  For defaults see the options in `manager_net_if_filter_option()`.

* __`manager_net_if_filter_module() = atom() <optional>`{: id=manager_ni_filter_module }__ - Module which handles the network interface filter part for the SNMP manager. Must implement the `m:snmpm_network_interface_filter` behaviour.

  Default is `snmpm_net_if_filter`.

* __`def_user_module() = atom() <optional>`{: id=manager_def_user_module }__ - The module implementing the default user. See the `m:snmpm_user` behaviour.

  Default is `snmpm_user_default`.

* __`def_user_data() = term() <optional>`{: id=manager_def_user_data }__ - Data for the default user. Passed to the user module when calling the callback functions.

  Default is `undefined`.

[](){: id=common_types }
Common config types:

* __`restart_type() = permanent | transient | temporary`{: id=restart_type }__ - See [supervisor](`m:supervisor#child_spec`) documentation for more info.

  Default is `permanent` for the agent and `transient` for the manager.

* __`db_init_error() = terminate | create | create_db_and_dir`{: id=db_init_error }__ - Defines what to do if the agent or manager is unable to open an existing database file. `terminate` means that the agent/manager will terminate and `create` means that the agent/manager will remove the faulty file(s) and create new ones, and `create_db_and_dir` means that the agent/manager will create the database file along with any missing parent directories for the database file.

  Default is `terminate`.

* __`priority() = atom() <optional>`{: id=prio }__ - Defines the Erlang priority for all SNMP processes.

  Default is `normal`.

* __`versions() = [version()] <optional>`{: id=versions }__ - `version() = v1 | v2 | v3`

  Which SNMP versions shall be accepted/used.

  Default is `[v1,v2,v3]`.

* __`verbosity() = silence | info | log | debug | trace <optional>`{: id=verbosity }__ - Verbosity for a SNMP process. This specifies now much debug info is printed.

  Default is `silence`.

* __`bind_to() = bool() <optional>`{: id=bind_to }__ - If `true`, net_if binds to the IP address. If `false`, net_if listens on any IP address on the host where it is running.

  Default is `false`.

* __`no_reuse() = bool() <optional>`{: id=no_reuse }__ - If `true`, net_if does not specify that the IP and port address should be reusable. If `false`, the address is set to reusable.

  Default is `false`.

* __`recbuf() = integer() <optional>`{: id=recbuf }__ - Receive buffer size.

  Default value is defined by `gen_udp`.

* __`sndbuf() = integer() <optional>`{: id=sndbuf }__ - Send buffer size.

  Default value is defined by `gen_udp`.

* __`extra_socket_options() = list() <optional>`{: id=extra_sock_opts }__ - A list of arbitrary socket options.

  This list is not inspected by snmp (other then checking that its a list). Its the users responsibility to ensure that these are valid options and does not conflict with the "normal" options.

  Default is `[]`.

* __`inet_backend() = inet | socket <optional>`{: id=inet_backend }__ - Choose the inet-backend.

  This option make it possible to use net_if (gen_udp) with a different inet-backend ('inet' or 'socket').

  Default is `inet`.

* __`note_store() = [note_store_opt()] <optional>`{: id=note_store }__ - `note_store_opt() = {timeout, note_store_timeout()} | {verbosity, verbosity()}`

  Specifies the start-up verbosity for the SNMP note store.

  For defaults see the options in `note_store_opt()`.

* __`note_store_timeout() = integer() <optional>`{: id=ns_timeout }__ - Note cleanup time. When storing a note in the note store, each note is given lifetime. Every `timeout` the note_store process performs a GC to remove the expired note's. Time in milli-seconds.

  Default is `30000`.

* __`audit_trail_log() = [audit_trail_log_opt()] <optional>`{: id=audit_trail_log }__ - `audit_trail_log_opt() = {type, atl_type()} | {dir, atl_dir()} | {size, atl_size()} | {repair, atl_repair()} | {seqno, atl_seqno()}`

  If present, this option specifies the options for the audit trail logging. The `disk_log` module is used to maintain a wrap log. If present, the `dir` and `size` options are mandatory.

  If not present, audit trail logging is not used.

* __`atl_type() = read | write | read_write <optional>`{: id=atl_type }__ - Specifies what type of an audit trail log should be used. The effect of the type is actually different for the the agent and the manager.

  For the agent:

  * If `write` is specified, only set requests are logged.
  * If `read` is specified, only get requests are logged.
  * If `read_write`, all requests are logged.

  For the manager:

  * If `write` is specified, only sent messages are logged.
  * If `read` is specified, only received messages are logged.
  * If `read_write`, both outgoing and incoming messages are logged.

  Default is `read_write`.

* __`atl_dir = dir() <mandatory>`{: id=atl_dir }__ - Specifies where the audit trail log should be stored.

  If `audit_trail_log` specifies that logging should take place, this parameter *must* be defined.

* __`atl_size() = {integer(), integer()} <mandatory>`{: id=atl_size }__ - Specifies the size of the audit trail log. This parameter is sent to `disk_log`.

  If `audit_trail_log` specifies that logging should take place, this parameter *must* be defined.

* __`atl_repair() = true | false | truncate | snmp_repair <optional>`{: id=atl_repair }__ - Specifies if and how the audit trail log shall be repaired when opened. Unless this parameter has the value `snmp_repair` it is sent to `disk_log`. If, on the other hand, the value is `snmp_repair`, snmp attempts to handle certain faults on its own. And even if it cannot repair the file, it does not truncate it directly, but instead *moves it aside* for later off-line analysis.

  Default is `true`.

* __`atl_seqno() = true | false <optional>`{: id=atl_seqno }__ - Specifies if the audit trail log entries will be (sequence) numbered or not. The range of the sequence numbers are according to RFC 5424, i.e. 1 through 2147483647.

  Default is `false`.

## See Also

application(3), disk_log(3)
