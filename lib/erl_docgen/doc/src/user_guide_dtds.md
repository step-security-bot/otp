# User's Guide DTDs

[](){: id=partDTD }
## The part DTD

The `part` DTD is intended for a "normal" document, like the User's Guide or Release Notes. First are some paragraphs introducing the main contents. After that follows chapters, written in separate files with the [chapter](user_guide_dtds.md#chapterdtd) DTD.

Example:

```text
<?xml version="1.0" encoding="latin1" ?>
<!DOCTYPE part SYSTEM "part.dtd">
<part>
  <header>
    <title>The chapter title</title>
    <prepared>The author</prepared>
    <docno/>
    <date/>
    <rev/>
  </header>

  <description>
    <p>Some text..</p>
  </description>

  <include file="file1"></include>
  <include file="file2"></include>
</part>
```

[](){: id=partTAG }
## <part>

The top level tag of a `part` DTD.

Contains a [<header>](header_tags.md), an optional [<description>](user_guide_dtds.md#descriptiontag), followed by one or more [<include>](user_guide_dtds.md#includetag).

[](){: id=descriptionTAG }
## <description>

The introduction after the title and before the bulk of included chapters/manual pages.

Contains any combination and any number of [block tags](block_tags.md) except `<image>` and `<table>`.

[](){: id=includeTAG }
## <include>

An empty tag. The attribute `file` specifies a file to include. The `.xml` file extension should be omitted.

Example:

```text
<include file="notes"></include>
```

[](){: id=chapterDTD }
## The chapter DTD

The `chapter` DTD is intended for a chapter in a User's Guide or similar with text divided into sections, which can be nested.

Example:

```text
<?xml version="1.0" encoding="latin1" ?>
<!DOCTYPE chapter SYSTEM "chapter.dtd">
<chapter>
  <header>
    <title>Title on first level</title>
    <prepared/>
    <docno/>
    <date/>
    <rev/>
  </header>
  
  <p>Introduction...</p>

  <section>
    <title>Title on second level</title>

    <p>First paragraph.</p>

    <p>Second paragraph etc.</p>

    <section>
      <title>Title on third level</title>

      <p>...</p>
    </section>
  </section>

  ...
</chapter>
```

[](){: id=chapterTAG }
## <chapter>

The top level tag of a `chapter` DTD.

Contains a [<header>](header_tags.md), an optional introduction consisting of any combination of [block tags](block_tags.md), followed by one or more [<section>](user_guide_dtds.md#sectiontag).

[](){: id=sectionTAG }
## <section>

Subdivision of a chapter.

Contains an optional [<marker>](inline_tags.md#markertag), a [<title>](user_guide_dtds.md#titletag), followed by any combination and any number of [block tags](block_tags.md) and `<section ghlink="maint/lib/erl_docgen/doc/src/user_guide_dtds.xml#L172">`.

[](){: id=titleTAG }
## <title>

Section title, contains plain text.
