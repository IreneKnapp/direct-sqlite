# Pull request guidelines

* Please add automated unit tests for any new features you add.  If you don't, expect to be asked to do so in PR review.

# Testing

Unit tests can be mostly found in [test/Main.hs](https://github.com/IreneKnapp/direct-sqlite/blob/master/test/Main.hs).

Running tests:

```
cabal sandbox init
cabal install --enable-tests --only-dep
cabal test
```

# Release checklist

1. Test `master locally`
2. Check that Travis CI build passes on the HEAD revision.
3. Update changelog.
4. Bump versions in the .cabal file.
5. Build an sdist.  Test the sdist in a separate sandbox.
6. Upload to Hackage
7. Upload docs to Hackage using https://github.com/ekmett/lens/blob/master/scripts/hackage-docs.sh
  * this step can be removed if Hackage starts building the docs automatically as it used to
8. Tag the current HEAD with `direct-sqlite-X.Y.Z` version (`git tag -a direct-sqlite-X.Y.Z`, `git push --tags`)

# Upgrading sqlite3

Be sure to also upgrade the sqlite extension headers (see https://github.com/IreneKnapp/direct-sqlite/pull/66 for the filenames).
