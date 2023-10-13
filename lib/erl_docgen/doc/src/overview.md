# Overview OTP DTDs

## DTD Suite

Input is written as XML according to one of the DTDs and output is corresponding HTML. Documentation for an Erlang/OTP application is usually organized as follows:

* __*User's Guide*__ - (DTD: [part](user_guide_dtds.md#partdtd)) A collection of chapters ([chapter](user_guide_dtds.md#chapterdtd)).

* __*Reference Manual*__ - (DTD: [application](refman_dtds.md#applicationdtd) A collection of manual pages for modules ([erlref](refman_dtds.md#erlrefdtd)), applications ([appref](refman_dtds.md#apprefdtd)), commands ([comref](refman_dtds.md#comrefdtd)), C libraries ([cref](refman_dtds.md#crefdtd)) and files ([fileref](refman_dtds.md#filerefdtd)).

* __*Release Notes*__ - Same structure as the User's Guide.

In some cases, one or more of the User's Guide, Reference Manual and Release Notes are omitted. Also, it is possible to use either the `application` or `part` DTD to write other types of documentation for the application.

The structure of the different documents and the meaning of the tags are explained. There are numerous examples of documentation source code.

For readability and simplicity, the examples have been kept as short as possible. For an example of what the generated HTML will look like, it is recommended to look at the documentation of an OTP application.

## Basic Tags

All DTDs in the OTP DTD suite share a basic set of tags. An author can easily switch from one DTD to another and still use the same basic tags. It is furthermore easy to copy pieces of information from one document to another, even though they do not use the same DTD.

The basic set of tags are divided into two categories: [block tags](block_tags.md) and [inline tags](inline_tags.md). Block tags typically define a separate block of information, like a paragraph or a list. Inline tags are typically used within block tags, for example a highlighted word within a paragraph.
