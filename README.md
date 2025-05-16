# Modern C++ JSON Library

This is a modern C++ library for working with JSON data. It aims to provide full support for the JSON standard, as well as allowing users to customize and extend the library according to their needs. The library offers a user-friendly and C++-idiomatic API, without compromising on performance.

This branch is the v2 version and is under development. For the older version, please visit the [master](https://github.com/YexuanXiao/basic_json/tree/master) branch.

## Goals

- Supports JSON Pointer, JSON Patch
- Supports UTF-8, UTF-16, and UTF-32 encodings
- Supports custom number type and integer extensions
- Supports JSON parsing, serialization, and manipulation
- Supports pretty-printing, indentation, and escaping options
- Supports user-defined types, custom allocators, and custom serializers

- Use exceptions and exception safety
- Use herbceptions when it is available

## Not Goals

- INF and NAN
- Reading comments
- Other extensions

## Roadmap

Stage 1: JSON representation, accessors and modifiers

Stage 2: Serializer and deserializer

Stage 3: JSON Pointer and Patch

## Proposal draft

https://storage.nykz.org/proposals/minimal-json/

![figure](https://raw.githubusercontent.com/YexuanXiao/basic_json/v2/figure.svg)
