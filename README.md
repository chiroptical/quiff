quiff
=====

An in-tray application written in Erlang. The planned components:

- a server to store tasks written in Erlang
    - use `mnesia` for storage
    - `gen_server` for handling interaction
    - `elli` for handling requests
- a command line client to interact with the server written in Rust (?)
- (maybe) a neovim plugin to interact with the server written in Fennel (?)

Motivation
-----

This is an educational clone of: https://github.com/NorfairKing/intray. It
seems like a small enough project to build.

Build
-----

    $ rebar3 compile
