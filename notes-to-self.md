# Notes to Self

I always forget how to generate stuff for Hackage (particularly with
documentation), so here are some notes.

## Creating and uploading a distribution

An example.

```console
cabal sdist
cabal upload ./dist-newstyle/sdist/mr-env-0.1.0.2.tar.gz
```

## Creating and uploading docs

Another example.

```console
cabal haddock --haddock-for-hackage --enable-doc
cabal upload -d ./dist-newstyle/mr-env-0.1.0.2-docs.tar.gz
```

Note that in these cases the distribution will be uploaded as a candidate. To
publish, add the `--publish` flag.
