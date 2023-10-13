# Introduction

## Scope

The Kernel application has all the code necessary to run the Erlang runtime system: file servers, code servers, and so on.

The Kernel application is the first application started. It is mandatory in the sense that the minimal system based on Erlang/OTP consists of Kernel and STDLIB. Kernel contains the following functional areas:

* Start, stop, supervision, configuration, and distribution of applications
* Code loading
* Logging
* Global name service
* Supervision of Erlang/OTP
* Communication with sockets
* Operating system interface

## Prerequisites

It is assumed that the reader is familiar with the Erlang programming language.
