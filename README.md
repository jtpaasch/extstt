# `extstt`

Simply typed lambda calculus extended with custom base types,
options, records, and lists.


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

To build the library (using `cabal` 3.0+):

    cabal build

Or:

    make

To import a particular library module into GHCi:

    Prelude> :l lib/Calculus/Extra/Examples.hs

To load all libraries into GHCi:

    cabal repl extstt 

To clean:

    cabal clean

Or:

    make clean

