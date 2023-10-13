# app

Application resource file.

## Description

The *application resource file* specifies the resources an application uses, and how the application is started. There must always be one application resource file called `Application.app` for each application `Application` in the system.

The file is read by the application controller when an application is loaded/started. It is also used by the functions in `systools`, for example when generating start scripts.

## File Syntax

The application resource file is to be called `Application.app`, where `Application` is the application name. The file is to be located in directory `ebin` for the application.

The file must contain a single Erlang term, which is called an *application specification*:

```text
{application, Application,
  [{description,  Description},
   {id,           Id},
   {vsn,          Vsn},
   {modules,      Modules},
   {maxP,         MaxP},
   {maxT,         MaxT},
   {registered,   Names},
   {included_applications, Apps},
   {optional_applications, Apps},
   {applications, Apps},
   {env,          Env},
   {mod,          Start},
   {start_phases, Phases},
   {runtime_dependencies, RTDeps}]}.

             Value                Default
             -----                -------
Application  atom()               -
Description  string()             ""
Id           string()             ""
Vsn          string()             ""
Modules      [Module]             []
MaxP         int()                infinity
MaxT         int()                infinity
Names        [Name]               []
Apps         [App]                []
Env          [{Par,Val}]          []
Start        {Module,StartArgs}   []
Phases       [{Phase,PhaseArgs}]  undefined
RTDeps       [ApplicationVersion] []

Module = Name = App = Par = Phase = atom()
Val = StartArgs = PhaseArgs = term()
ApplicationVersion = string()
```

* __`Application`__ - Application name.

For the application controller, all keys are optional. The respective default values are used for any omitted keys.

The functions in `systools` require more information. If they are used, the following keys are mandatory:

* `description`
* `vsn`
* `modules`
* `registered`
* `applications`

The other keys are ignored by `systools`.

* __`description`__ - A one-line description of the application.

* __`id`__ - Product identification, or similar.

* __`vsn`__ - Version of the application.

* __`modules`__ - All modules introduced by this application. `systools` uses this list when generating start scripts and tar files. A module can only be defined in one application.

* __`maxP`__ - *Deprecated - is ignored*

  Maximum number of processes allowed in the application.

* __`maxT`__ - Maximum time, in milliseconds, that the application is allowed to run. After the specified time, the application terminates automatically.

* __`registered`__ - All names of registered processes started in this application. `systools` uses this list to detect name clashes between different applications.

* __`included_applications`__ - All applications included by this application. When this application is started, all included applications are loaded automatically, but not started, by the application controller. It is assumed that the top-most supervisor of the included application is started by a supervisor of this application.

* __`applications`__ - All applications that must be started before this application. If an application is also listed in `optional_applications`, then the application is not required to exist (but if it exists, it is also guaranteed to be started before this one).

  `systools` uses this list to generate correct start scripts. Defaults to the empty list, but notice that all applications have dependencies to (at least) Kernel and STDLIB.

* __`optional_applications`__ - A list of `applications` that are optional. Note if you want an optional dependency to be automatically started before the current application whenever it is available, it must be listed on both `applications` and `optional_applications`.

* __`env`__ - Configuration parameters used by the application. The value of a configuration parameter is retrieved by calling `application:get_env/1,2`. The values in the application resource file can be overridden by values in a configuration file (see [`config(4)`](config.md)) or by command-line flags (see [`erts:erl(1)`](`p:erts:erl_cmd.md`)).

* __`mod`__ - Specifies the application callback module and a start argument, see `m:application`.

  Key `mod` is necessary for an application implemented as a supervision tree, otherwise the application controller does not know how to start it. `mod` can be omitted for applications without processes, typically code libraries, for example, STDLIB.

* __`start_phases`__ - A list of start phases and corresponding start arguments for the application. If this key is present, the application master, in addition to the usual call to `Module:start/2`, also calls `Module:start_phase(Phase,Type,PhaseArgs)` for each start phase defined by key `start_phases`. Only after this extended start procedure, `application:start(Application)` returns.

  Start phases can be used to synchronize startup of an application and its included applications. In this case, key `mod` must be specified as follows:

  ```text
  {mod, {application_starter,[Module,StartArgs]}}
  ```

  The application master then calls `Module:start/2` for the primary application, followed by calls to `Module:start_phase/3` for each start phase (as defined for the primary application), both for the primary application and for each of its included applications, for which the start phase is defined.

  This implies that for an included application, the set of start phases must be a subset of the set of phases defined for the primary application. For more information, see [OTP Design Principles](`p:system:applications.md`).

* __`runtime_dependencies`{: id=runtime_dependencies }__ - A list of application versions that the application depends on. An example of such an application version is `"kernel-3.0"`. Application versions specified as runtime dependencies are minimum requirements. That is, a larger application version than the one specified in the dependency satisfies the requirement. For information about how to compare application versions, see section [Versions](`p:system:versions.md`) in the System Principles User's Guide.

  Notice that the application version specifies a source code version. One more, indirect, requirement is that the installed binary application of the specified version is built so that it is compatible with the rest of the system.

  Some dependencies can only be required in specific runtime scenarios. When such optional dependencies exist, these are specified and documented in the corresponding "App" documentation of the specific application.

  > #### Warning {: class=warning }
  > The `runtime_dependencies` key was introduced in OTP 17.0. The type of its value might be subject to changes during the OTP 17 release.

  > #### Warning {: class=warning }
  > All runtime dependencies specified in OTP applications during the OTP 17 release may not be completely correct. This is actively being worked on. Declared runtime dependencies in OTP applications are expected to be correct in OTP 18.

## See Also

`m:application`, `m:systools`
