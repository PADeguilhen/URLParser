# parseURL

A small URL parser written in Haskell, built on a hand-made parser combinator library.

It splits a URL such as

```
https://www.example.com/a/b?x=1&y=2#top
```

into its parts: scheme, domain (TLD, second-level domain, subdomains), path, query parameters and fragment.

## Requirements

- [GHC](https://www.haskell.org/ghc/) 9.2 or later (the project uses `GHC2021`)
- [cabal-install](https://www.haskell.org/cabal/) 3.0 or later

## Running the project

### Build

```sh
cabal build all
```

### Run the executable

```sh
cabal run parseURL
```

### Run the tests

```sh
cabal test
```

### Open a REPL

```sh
cabal repl parseURL     # library (Parse, URL)
cabal repl exe:myapp    # executable
```

Example in the REPL:

```haskell
ghci> import Parse
ghci> import URL
ghci> parseStr parseURL "https://www.example.com/a/b?x=1#top"
```

## Dependencies

| Package       | Used by     |
|---------------|-------------|
| `bytestring`  | Parse lib   |
| `utf8-string` | Parse lib   |
| `QuickCheck`  | test suite  |

Cabal downloads them automatically on the first build.
