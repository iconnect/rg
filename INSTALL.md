# Building and Installing

To prepare a checked-out work tree for building with the default
configuration, from the root folder:

    ./configure

(There may be other options: see `./configure --info` for
details.)

From there you can build with `stack build` or `cabal v2-build` (or install
with `stack install` or `cabal v2-build` -- you might need [update your
cabal installation](https://www.haskell.org/cabal/) if the `v2-build`
command is not recognised.)
