# rel

Release resource file

## Description

The *release resource file* specifies which applications are included in a release (system) based on Erlang/OTP.

This file is used by the functions in `m:systools` when generating start scripts (`.script`, `.boot`) and release upgrade files (`relup`).

## File Syntax

The release resource file is to be called `Name.rel`.

The `.rel` file contains one single Erlang term, which is called a *release specification*. The file has the following syntax:

```text
{release, {RelName,Vsn}, {erts, EVsn},
  [{Application, AppVsn} |
   {Application, AppVsn, Type} |
   {Application, AppVsn, IncApps} |
   {Application, AppVsn, Type, IncApps}]}.
```

* __`RelName = string()`__ - Release name.

* __`Vsn = string()`__ - Release version.

* __`EVsn = string()`__ - ERTS version the release is intended for.

* __`Application = atom()`__ - Name of an application included in the release.

* __`AppVsn = string()`__ - Version of an application included in the release.

* __`Type = permanent | transient | temporary | load | none`__ - Start type of an application included in the release.

  If `Type = permanent | transient | temporary`, the application is loaded and started in the corresponding way, see `m:application`.

  If `Type = load`, the application is only loaded.

  If `Type = none`, the application is not loaded and not started, although the code for its modules is loaded.

  Defaults to `permanent`

* __`IncApps = [atom()]`__ - A list of applications that are included by an application included in the release. The list must be a subset of the included applications specified in the application resource file (`Application.app`) and overrides this value. Defaults to the same value as in the application resource file.

> #### Note {: class=info }
> The list of applications must contain the Kernel and STDLIB applications.

## See Also

`m:application`, [`relup(4)`](relup.md), `m:systools`
