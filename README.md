Low-level SQLite3 bindings for Haskell
======================================

[![Build Status](https://travis-ci.org/IreneKnapp/direct-sqlite.png?branch=master)](https://travis-ci.org/IreneKnapp/direct-sqlite) ![Hackage](https://img.shields.io/hackage/v/direct-sqlite.svg?style=flat-square)

This package is not very different from the other SQLite3 bindings out there, but it fixes a few deficiencies I was finding. As compared to bindings-sqlite3, it is slightly higher-level, in that it supports marshalling of data values to and from the database. In particular, it supports strings encoded as UTF8, and BLOBs represented as ByteStrings.

For contribtions, please read [hacking on direct-sqlite](https://github.com/IreneKnapp/direct-sqlite/wiki/Hacking) before sending PRs.

# Contributors

- [Irene Knapp](https://github.com/nurpax) author
- [Janne Hellsten](https://github.com/nurpax) long-term maintainer
- [Sergey Bushnyak](https://github.com/sigrlami) current maintainer
