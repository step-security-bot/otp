# Introduction

[](){: id="erlang ref manual" }
This section is the Erlang reference manual. It describes the Erlang programming language.

## Purpose

The focus of the Erlang reference manual is on the language itself, not the implementation of it. The language constructs are described in text and with examples rather than formally specified. This is to make the manual more readable. The Erlang reference manual is not intended as a tutorial.

Information about implementation of Erlang can, for example, be found, in the following:

* [System Principles](`p:system:system_principles.md`)

  Starting and stopping, boot scripts, code loading, [logging](`p:system:error_logging.md`), [creating target systems](`p:system:create_target.md`)
* [Efficiency Guide](`p:system:advanced.md`)

  Memory consumption, system limits
* ERTS User's Guide

  [Crash dumps](`p:erts:crash_dump.md`), [drivers](`p:erts:driver.md`)

## Prerequisites

It is assumed that the reader has done some programming and is familiar with concepts such as data types and programming language syntax.

## Document Conventions

In this section, the following terminology is used:

* A *sequence* is one or more items. For example, a clause body consists of a sequence of expressions. This means that there must be at least one expression.
* A *list* is any number of items. For example, an argument list can consist of zero, one, or more arguments.

If a feature has been added in R13A or later, this is mentioned in the text.

## Complete List of BIFs

For a complete list of BIFs, their arguments and return values, see `m:erlang` manual page in ERTS.

## Reserved Words

The following are reserved words in Erlang:

`after and andalso band begin bnot bor bsl bsr bxor case catch cond div end fun if let maybe not of or orelse receive rem try when xor`

__Note__: `cond` and `let`, while reserved, are currently not used by the language.

> #### Change {: class=neutral }
> `maybe` is a reserved word only if feature `maybe_expr` is enabled. In Erlang/OTP 25 and 26, `maybe_expr` is disabled by default. Starting from Erlang/OTP 27, `maybe_expr` is enabled by default.
