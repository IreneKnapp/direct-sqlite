#!/usr/bin/env bash

set -xeu

# example usage: $0 https://www.sqlite.org/2022/sqlite-amalgamation-3370200.zip

url="$1"
projdir=$(realpath $(dirname $0)/..)

tmpdir=$(mktemp -d) && {
  trap "rm -rf $tmpdir" EXIT
  cd "$tmpdir"
  curl -O "$url"
  unzip *.zip
  cp sqlite-*/{sqlite3.c,sqlite3ext.h,sqlite3.h} "$projdir/cbits/"
}
