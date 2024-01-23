Low-level SQLite3 bindings for Haskell
======================================

[![Build Status](https://github.com/IreneKnapp/direct-sqlite/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/IreneKnapp/direct-sqlite/actions/workflows/haskell-ci.yml) [![Hackage](https://img.shields.io/hackage/v/direct-sqlite.svg)](https://hackage.haskell.org/package/direct-sqlite)

This package is not very different from the other SQLite3 bindings out there, but it fixes a few deficiencies I was finding. As compared to bindings-sqlite3, it is slightly higher-level, in that it supports marshalling of data values to and from the database. In particular, it supports strings encoded as UTF8, and BLOBs represented as ByteStrings.

For contribtions, please read [contributing guide](CONTRIBUTING.md) before sending PRs.

# Contributors

- [Irene Knapp](https://github.com/nurpax) author
- [Janne Hellsten](https://github.com/nurpax) long-term maintainer
- [Sergey Bushnyak](https://github.com/sigrlami) long-term maintainer
- [Joshua Chia](https://github.com/jchia) current maintainer
