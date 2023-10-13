# Erlang Syntax and Metaprogramming tools

## Overview

This package contains modules for handling abstract syntax trees (ASTs) in Erlang, in a way that is compatible with the "abstract format" parse trees of the stdlib module `erl_parse`, together with utilities for reading source files, [pretty-printing syntax trees](`m:erl_prettypr`), and doing [metaprogramming](`m:merl`) in Erlang.

The abstract layer (defined in `m:erl_syntax`) is nicely structured and the node types are context-independent. The layer makes it possible to transparently attach source-code comments and user annotations to nodes of the tree. Using the abstract layer makes applications less sensitive to changes in the `m:erl_parse` data structures, only requiring the `erl_syntax` module to be up-to-date.

The pretty printer `m:erl_prettypr` is implemented on top of the library module `m:prettypr`: this is a powerful and flexible generic pretty printing library, which is also distributed separately.

For a short demonstration of parsing and pretty-printing, simply compile the included module [demo.erl](demo.erl), and execute `demo:run()` from the Erlang shell. It will compile the remaining modules and give you further instructions.
