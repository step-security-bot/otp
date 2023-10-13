# Block Tags

Block tags typically define a separate block of information, such as a paragraph or a list.

The following subset of block tags are common for all DTDs in the OTP DTD suite: [](){: id=block_subset }
[<p>](block_tags.md#ptag), [<pre>](block_tags.md#pretag), [<code>](block_tags.md#codetag), [<list>](block_tags.md#listtag), [<taglist>](block_tags.md#taglisttag) and [<codeinclude>](block_tags.md#codeincludetag)

[](){: id=brTAG }
## <br> - Line Break

Forces a newline. Example:

```text
Eat yourself<br/>senseless!
```

results in:

Eat yourself  
senseless\!

The `<br>` tag is both a block- and an inline tag.

[](){: id=codeTAG }
## <code> - Code Example

Highlight code examples. Example:

```text
<code>
sum([H|T]) ->
    H + sum(T);
sum([]) ->
    0.
</code>
```

results in:

```text
sum([H|T]) ->
    H + sum(T);
sum([]) ->
    0.
```

There is an attribute `type = "erl" | "c" | "none"`, but currently this attribute is ignored. Default value is `"none"`

> #### Note {: class=info }
> No tags are allowed within the tag and no [character entities](character_entities.md) are expanded.

[](){: id=codeincludeTAG }
## <codeinclude> - Code Inclusion

Include external code snippets. The attribute `file` gives the file name and `tag` defines a string which delimits the code snippet. Example:

```text
<codeinclude file="example.txt" tag="%% Erlang example"/>
```

results in:

```text

-module(example).

start() ->
    {error,"Pid required!"}.

start(Pid) ->
    spawn(smalltalk,main,[]).
```

provided there is a file named `examples.txt` looking like this:

```text
...

%% Erlang example
-module(example).

start() ->
    {error,"Pid required!"}.
start(Pid) ->
    spawn(fun() -> init(Pid) end).
%% Erlang example

...
```

If the `tag` attribute is omitted, the whole file is included.

There is also an attribute `type = "erl" | "c" | "none"`, but currently this attribute is ignored. Default value is `"none"`

[](){: id=listTAG }
## <list> - List

The attribute `type = "ordered"|"bulleted"` decides if the list is numbered or bulleted. Default is `"bulleted"`.

Lists contains list items, tag `<item>`, which can contain plain text, the [common subset of block tags](block_tags.md#block_subset) and [inline tags](inline_tags.md). Example:

```text
<list type="ordered">
  <item>Askosal:
    <list>
      <item>Nullalisis</item>
      <item>Facilisis</item>
    </list>
  </item>
  <item>Ankara</item>
</list>
```

results in:

1. Askosal:

  * Nullalisis
  * Facilisis
1. Ankara

[](){: id=markerTAG }
## <marker> - Marker

Used as an anchor for hypertext references. The `<marker>` tag is both a block- and an inline tag and is described in the [Inline Tags](inline_tags.md#markertag) section.

[](){: id=pTAG }
## <p> - Paragraph

Paragraphs contain plain text and [inline tags](inline_tags.md). Example:

```text
<p>I call specific attention to
  the authority given by the <em>21st Amendment</em>
  to the Constitution to prohibit transportation
  or importation of intoxicating liquors into
  any State in violation of the laws of such
  State.</p>
```

results in:

I call specific attention to the authority given by the *21st Amendment* to the Constitution to prohibit transportation or importation of intoxicating liquors into any State in violation of the laws of such State.

[](){: id=noteTAG }
## <note> - Note

Highlights a note. Can contain block tags except `<note>`, `<warning>`, `<image>` and `<table>`. Example:

```text
<note>
  <p>This function is mainly intended for debugging.</p>
</note>
```

results in:

> #### Note {: class=info }
> This function is mainly intended for debugging.

[](){: id=preTAG }
## <pre> - Pre-formatted Text

Used for documentation of system interaction. Can contain text, [<see*> tags](inline_tags.md#seetag), [<url>](inline_tags.md#urltag) and `<input>` tags.

The `<input>` tag is used to highlight user input. Example:

```text
<pre>
$ <input>erl</input>
Erlang (BEAM) emulator version 5.5.3 [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.5.3  (abort with ^G)
1> <input>pwd().</input>
/home/user
2> <input>halt().</input>
</pre>
```

results in:

```text
$ erl
Erlang (BEAM) emulator version 5.5.3 [async-threads:0] [hipe] [kernel-poll:false]

Eshell V5.5.3  (abort with ^G)
1> pwd().
/home/user
2> halt().
```

All [character entities](character_entities.md) are expanded.

[](){: id=quoteTAG }
## <quote> - Quotation

Highlight quotations from other works, or dialog spoken by characters in a narrative. Contains one or more [<p>](block_tags.md#ptag) tags. Example:

```text
<p>Whereas Section 217(a) of the Act of Congress entitled
"An Act ..." approved June 16, 1933, provides as follows:</p>
<quote>
  <p>Section 217(a) The President shall proclaim the law.</p>
</quote>
```

results in:

Whereas Section 217(a) of the Act of Congress entitled "An Act ..." approved June 16, 1933, provides as follows:

> Section 217(a) The President shall proclaim the law.

[](){: id=taglistTAG }
[](){: id=tagTAG }
## <taglist> - Definition List

Definition lists contains pairs of tags, `<tag>`, and list items, `<item>`.

`<tag>` can contain plain text, [<c>](inline_tags.md#ctag), [<em>](inline_tags.md#emtag), [<see*> tags](inline_tags.md#seetag) and [<url>](inline_tags.md#urltag) tags.

`<item>` can contain plain text, the [common subset of block tags](block_tags.md#block_subset) and [inline tags](inline_tags.md). Example:

```text
<taglist>
  <tag><c>eacces</c></tag>
  <item>Permission denied.</item>
  <tag><c>enoent</c></tag>
  <item>No such file or directory.</item>
</taglist>
```

results in:

* __`eacces`__ - Permission denied.

* __`enoent`__ - No such file or directory.

[](){: id=warningTAG }
## <warning> - Warning

Highlights a warning. Can contain block tags except `<note>`, `<warning>`, `<image>` and `<table>`. Example:

```text
<warning>
  <p>This function might be removed in a future version without
    prior warning.</p>
</warning>
```

results in:

> #### Warning {: class=warning }
> This function might be removed in a future version without prior warning.

[](){: id=imageTAG }
[](){: id=icaptionTAG }
## <image> - Image

Graphics is imported using the `<image>` tag. An image caption `<icaption>`, containing plain text, must be supplied. Example:

```text
<image file="man">
  <icaption>A Silly Man</icaption>
</image>
```

results in:

![A Silly Man](assets/man.gif "A Silly Man")

This assumes that `man.gif` exists in the current directory.

[](){: id=tableTAG }
[](){: id=rowTAG }
[](){: id=cellTAG }
[](){: id=tcaptionTAG }
## <table> - Table

The table format is similar to how tables are described in HTML 3.2. A table contains one or more rows, `<row>`, and a table caption `<tcaption>`, containing plain text.

Each row contains one or more cells, `<cell>`. The attributes `align = "left"|"center"|"right"` and `valign = "top"|"middle"|"bottom"` decides how text is aligned in the cell horizontally and vertically. Default is "`left`" and "`middle`".

Each cell contains plain text and [inline tags](inline_tags.md). Example:

```text
    <table>
      <row>
        <cell align="left" valign="top"><em>Boys</em></cell>
        <cell align="center" valign="middle"><em>Girls</em></cell>
      </row>
      <row>
        <cell align="left" valign="middle">Juda</cell>
        <cell align="right" valign="bottom">Susy</cell>
      </row>
      <row>
        <cell align="left" valign="middle">Anders</cell>
        <cell align="left" valign="middle">Victoria</cell>
      </row>
      <tcaption>A table caption</tcaption>
    </table>
```

results in:

| *Boys* | *Girls* |
|--------|---------|
| Juda | Susy |
| Anders | Victoria |


*Table: A table caption*
