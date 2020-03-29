# `extstt`

Simply typed lambda calculus extended with custom base types,
option types, and records.


## Build

Download the project, e.g.:

    git clone https://..../extstt.git
    cd extstt 

Build it:

    make build

To clean:

    make clean


## Development

Download the project, e.g.:

    git clone https://..../extstt.git
    cd extstt 

To build the library:

    cabal new-build

To import a particular library module into GHCi:

    Prelude> :l lib/Calculus/Extra/Examples.hs

To load all libraries into GHCi:

    cabal repl extstt 

To clean:

    cabal new-clean


