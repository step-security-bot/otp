# Reference Manual DTDs

There are five DTDs for writing manual pages about applications, shell commands, C libraries, Erlang modules and files, all with a similar structure:

* A header.
* Name of the application/command/library/module/file.
* Short summary (one line).
* A longer description.
* "Formal" definitions of functions or commands.
* Optional sections of free text.
* Optional section with the name(s) and email(s) of the author(s).

The differences between the DTDs are the tags for the name, the short summary and some tags inside the "formal" definitions.

[](){: id=applicationDTD }
## The application DTD

The `application` DTD is intended for a Reference Manual and groups a set of manual pages into one unit. The structure is similar to the part DTD: first an introduction and then the manual pages, written in separate files with the [appref](refman_dtds.md#apprefdtd), [comref](refman_dtds.md#comrefdtd), [cref](refman_dtds.md#crefdtd), [erlref](refman_dtds.md#erlrefdtd), or [fileref](refman_dtds.md#filerefdtd) DTD.

Example:

```text
<?xml version="1.0" encoding="latin1" ?>
<!DOCTYPE application SYSTEM "application.dtd">
<application>
  <header>
    <title>Application name</title>
    <prepared/>
    <docno/>
    <date/>
    <rev/>
  </header>

  <description>
    <p>Application description...</p>
  </description>
  
  <include file="module1">
  <include file="module2">
</application>
```

[](){: id=applicationTAG }
## <application>

The top level tag of an `application` DTD.

Contains a [<header>](header_tags.md), an optional [<description>](user_guide_dtds.md#descriptiontag), followed by one or more [<include>](user_guide_dtds.md#includetag).

[](){: id=apprefDTD }
## The appref DTD

This is the DTD for writing an application manual page.

Example:

```text
<?xml version="1.0" encoding="latin1" ?>
<!DOCTYPE appref SYSTEM "appref.dtd">
<appref>
  <header>
    <title>Application name</title>
    <prepared/>
    <docno/>
    <date/>
    <rev/>
  </header>

  <app>Application name</app>

  <appsummary>A short application summary.</appsummary>

  <description>
    <p>A longer description of the application.</p>
  </description>
  
  <section>
    <title>Configuration</title>

      <p>...</p>
  </section>

  ...
  
  <authors>
    <aname>Name of author</aname>
    <email>Email of author</email>
  </authors>
</appref>
```

[](){: id=apprefTAG }
### <appref>

The top level tag of an `appref` DTD.

Contains [<header>](header_tags.md#headertag), [<app>](refman_dtds.md#apptag), [<appsummary>](refman_dtds.md#appsummarytag), [<description>](refman_dtds.md#descriptiontag), zero or more [<section>](refman_dtds.md#sectiontag) and [<funcs>](refman_dtds.md#funcstag), followed by zero or more [<authors>](refman_dtds.md#authorstag).

[](){: id=appTAG }
### <app>

The application name. Contains plain text.

[](){: id=appsummaryTAG }
### <appsummary>

Short summary. Contains plain text.

[](){: id=comrefDTD }
## The comref DTD

This is the DTD for writing a command manual page.

Example:

```text
<?xml version="1.0" encoding="latin1" ?>
<!DOCTYPE comref SYSTEM "comref.dtd">
<comref>
  <header>
    <title>Command name</title>
    <prepared/>
    <docno/>
    <date/>
    <rev/>
  </header>

  <com>Command name</com>

  <comsummary>A short command summary.</comsummary>

  <description>
    <p>A long description of the command.</p>
  </description>
  
  <funcs>
    <func>
      <name>command</name>
      <name>command -flag <arg></name>
      <fsummary>A short command summary (max 40 characters).</fsummary>
      <desc>
        <p>An extended command description.
      </desc>
    </func>
  </funcs>

  <section>
    <title>Options</title>

    <p>...</p>
  </section>
  
  <authors>
    <aname>Name of author</aname>
    <email>Email of author</email>
  </authors>
</comref>
```

[](){: id=comrefTAG }
### <comref>

The top level tag for a `comref` DTD.

Contains [<header>](header_tags.md#headertag), [<com>](refman_dtds.md#comtag), [<comsummary>](refman_dtds.md#comsummarytag), [<description>](refman_dtds.md#descriptiontag), zero or more [<section>](refman_dtds.md#sectiontag) and [<funcs>](refman_dtds.md#funcstag), followed by zero or more [<authors>](refman_dtds.md#authorstag).

[](){: id=comTAG }
### <com>

The command name. Contains plain text.

[](){: id=comsummaryTAG }
### <comsummary>

Short summary. Contains plain text.

[](){: id=crefDTD }
## The cref DTD

This is the DTD for writing a C library manual page.

Example:

```text
<?xml version="1.0" encoding="latin1" ?>
<!DOCTYPE cref SYSTEM "cref.dtd">
<cref>
  <header>
    <title>C library name</title>
    <prepared/>
    <docno/>
    <date/>
    <rev/>
  </header>

  <lib>C library name</lib>

  <libsummary>A short C library summary.</libsummary>

  <description ghlink="maint/lib/erl_docgen/doc/src/refman_dtds.xml#L285">
    <p>A longer description of the C library.</p>
  </description>
  
  <funcs>
    <func ghlink="maint/lib/erl_docgen/doc/src/refman_dtds.xml#L290">
      <name><ret>void</ret><nametext>start(bar)</nametext></name>
      <name><ret>void</ret><nametext>start(foo)</nametext></name>
      <fsummary>A short function summary (max 40 characters).</fsummary>
      <type>
        <v>char bar</v>
        <v>int foo</v>
      </type>
      <desc>
        <p>An extended function description.</p>
      </desc>
    </func>

    ...
  </funcs>

  <section ghlink="maint/lib/erl_docgen/doc/src/refman_dtds.xml#L306">
    <title>A title</title>

    <p>Some text...</p>
  </section>
  
  
</cref>
```

[](){: id=crefTAG }
### <cref>

The top level tag for a `cref` DTD.

Contains [<header>](header_tags.md#headertag), [<lib>](refman_dtds.md#libtag), [<libsummary>](refman_dtds.md#libsummarytag), [<description>](refman_dtds.md#descriptiontag), zero or more [<section>](refman_dtds.md#sectiontag) and [<funcs>](refman_dtds.md#funcstag), followed by zero or more [<authors>](refman_dtds.md#authorstag).

[](){: id=libTAG }
### <lib>

The C library name or acronym. Contains plain text.

[](){: id=libsummaryTAG }
### <libsummary>

Short summary. Contains plain text.

[](){: id=erlrefDTD }
## The erlref DTD

This is the DTD for writing Erlang module manual pages.

Example:

```text
<?xml version="1.0" encoding="latin1" ?>
<!DOCTYPE erlref SYSTEM "erlref.dtd">
<erlref>
  <header>
    <title>Module name</title>
    <prepared/>
    <docno/>
    <date/>
    <rev/>
  </header>

  <module>Module name</module>

  <modulesummary>A short module summary.</modulesummary>

  <description>
    <p>A longer description of the module.</p>
  </description>
  
  <funcs>
    <func>
      <name>start() -> Result</name>
      <name>start(N) -> Result</name>
      <fsummary>A short function summary (max 40 characters).</fsummary>
      <type>
        <v>Pid = pid()</v>
        <v>N = int()</v>
        <v>Result = {ok, Pid} | {error, Reason}</v>
        <v>Reason = term()</v>
        <d>A parameter description.</d>
      </type>
      <desc>
        <p>An extended function description.</p>
      </desc>
    </func>

    ...
  </funcs>

  <section>
    <title>Some Title</title>
    <p>Some text...</p>
  </section>
  
  <authors>
    <aname>Name of author</aname>
    <email>Email of author</email>
  </authors>
</erlref>
```

[](){: id=erlrefTAG }
### <erlref>

The top level tag for an `erlref` DTD.

Contains [<header>](header_tags.md#headertag), [<module>](refman_dtds.md#moduletag), [<modulesummary>](refman_dtds.md#modulesummarytag), [<description>](refman_dtds.md#descriptiontag), zero or more [<section>](refman_dtds.md#sectiontag) and [<funcs>](refman_dtds.md#funcstag), followed by zero or more [<authors>](refman_dtds.md#authorstag).

[](){: id=moduleTAG }
### <module>

The module name. Contains plain text.

[](){: id=modulesummaryTAG }
### <modulesummary>

Short summary. Contains plain text.

[](){: id=filerefDTD }
## The fileref DTD

This is the DTD for writing file manual pages. In OTP, this DTD is used for defining the format of for example `.rel` and `.app` files.

Example:

```text
<?xml version="1.0" encoding="latin1" ?>
<!DOCTYPE fileref SYSTEM "fileref.dtd">
<fileref>
  <header>
    <title>File name</title>
    <prepared/>
    <docno/>
    <date/>
    <rev/>
  </header>

  <file>fileref</file>

  <filesummary>A short file summary.</filesummary>

  <description>
    <p>A longer description of the file.</p>
  </description>
  
  <section>
    <title>File format</title>

    <p>...</p>
  </section>
  
  <authors>
    <aname>Name of author</aname>
    <email>Email of author</email>
  </authors>
</fileref>
```

The file reference manual can also contain function definitions, similar to the `erlref` DTD.

[](){: id=filerefTAG }
### <fileref>

The top level tag for a `fileref` DTD.

Contains [<header>](header_tags.md#headertag), [<file>](refman_dtds.md#filetag), [<filesummary>](refman_dtds.md#filesummarytag), [<description>](refman_dtds.md#descriptiontag), zero or more [<section>](refman_dtds.md#sectiontag) and [<funcs>](refman_dtds.md#funcstag), followed by zero or more [<authors>](refman_dtds.md#authorstag).

[](){: id=fileTAG }
### <file>

The name of the file or file type. Contains plain text.

[](){: id=filesummaryTAG }
### <filesummary>

Short summary. Contains plain text.

[](){: id=descriptionTAG }
## <description>

The introduction after the title and before sections and "formal" definitions.

Contains any combination and any number of [block tags](block_tags.md) except `<image>` and `<table>`.

[](){: id=sectionTAG }
## <section>

Subdivisions of the document. Contains an optional [<marker>](inline_tags.md#markertag), a [<title>](user_guide_dtds.md#titletag), followed by any combination and any number of [block tags](block_tags.md) except `<image>` and `<table>`.

[](){: id=funcsTAG }
## <funcs>

A group of "formal" function definitions.

Contains one or more [<func>](refman_dtds.md#functag).

[](){: id=funcTAG }
## <func>

A "formal" function definition.

Contains one or more [<name>](refman_dtds.md#nametag), followed by [<fsummary>](refman_dtds.md#fsummarytag), [<type>](refman_dtds.md#typetag) (optional) and [<desc>](refman_dtds.md#desctag) (optional).

[](){: id=nameTAG }
## <name>

Function/command signature with name, arguments and return value. Contains plain text, except for the `cref` DTD where it contains a `<ret>` (return type, plain text) and a `<nametext>` (function name and arguments, plain text).

In the case of an `erlref` DTD, it will automatically be added a [marker](inline_tags.md#markertag), `<marker id="Name/Arity">` or `<marker id="Name">`, based on the contents of this tag before the function definition.

Example: Consider the following name definition

```text
<name>foo(Arg1, Arg2) -> ok | {error, Reason}</name>
```

Then a marker like this will be added `<marker id="foo/2">` before the function definition in the generated HTML. That is, referring to the function using `<seemfa marker="#foo/2">foo/2</seemfa>` will automatically work.

[](){: id=fsummaryTAG }
## <fsummary>

Function/command summary. Contains plain text, [<c>](inline_tags.md#ctag) and [<em>](inline_tags.md#emtag).

[](){: id=typeTAG }
## <type>

Type declarations for the function/command.

Contains one or more pairs of [<v>](refman_dtds.md#vtag) and [<d>](refman_dtds.md#dtag) (optional).

[](){: id=vTAG }
## <v>

Type declaration for an argument or return value. Contains plain text.

[](){: id=dTAG }
## <d>

Description for an argument or return value. Contains plain text, [<c>](inline_tags.md#ctag) and [<em>](inline_tags.md#emtag).

[](){: id=descTAG }
## <desc>

Function/command description. Contains [block tags](block_tags.md) except `<image>` and `<table>`.

[](){: id=authorsTAG }
## <authors>

Authors of the manual page. The `authors` element is optional.

Contains one or more pairs of [<aname>](refman_dtds.md#anametag) and [<email>](refman_dtds.md#emailtag).

[](){: id=anameTAG }
## <aname>

Author name. Contains plain text.

[](){: id=emailTAG }
## <email>

Author email address. Contains plain text.
