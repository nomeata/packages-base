Base splitting experiment
=========================

On this branch, based on the ghc-7.6 branch, it is tested how far base can be
split into smaller components that separate concerns. Main goals are:
 
 * Reduce the number of API breaks for packages, by allowing them to specify their
   dependencies more exact.
 * Having a base pure package that does not include IO or Foreign stuff, with an API
   that can also be provided by strange targets (JS, etc.)
 * Separating out floating point stuff might be interesting as well.

Setup 
-----

This branch still contains the base package. But it also contains
base-something packages as subdirectories, with symbolic links into the main
directory, to verify that the individual components build.

How to test
-----------

    mkdir /tmp/base-split/ 
    ghc-pkg recache --package-db=/tmp/base-split/
    cabal install base-*/ --disable-library-profiling --package-db=clear --package-db=global --package-db=/tmp/base-split/ --prefix=/tmp/base-split/ --disable-documentation --force-reinstalls


Changes so far
--------------

These packages have been introduced so far:

 * base-pure: Basic stuff without `IO`, `Foreign` or floating point arithmetic. 
 * base-io: The `IO` and `ST` monads. `ST` could be separated as well.
 * base-foreign: Everything related to `Foreign`.

Some changes are intended to end up in the final libraries, if this approach is
accepted:

 * More `{- LANGUAGE NoImplicitPrelude -}` and more specific imports
 * Some reorganisation, e.g. `Data.Int.Show` pulled out of `Numeric` and `Foreign.C.Puts` pulled out of `System.Posix.Internals`
 * A pure implemenation of `GHC.Fingerprint` (but not this one, see [http://www.haskell.org/pipermail/glasgow-haskell-users/2013-February/023784.html] and follow ups)
 * Introduction of new `Exceptions` that used to be `IOExceptions`, but need to exist before `IOHandles` exist:
   * `IOFail`, `ErrnoError`, `OOMException`, `CodingError`

Some changes are debatable:

 * Removing of `Read` from `base-pure`. Probably not needed.

Some changes are just work-arounds due to GHC having the package name `base` hardcoded:

 * Lots of `fromInteger` and `negate`, to explicitly use the `Num` cass from `pure-base`
 * Changes from `deriving` to proper (but empty) class declarations.
 * Replacing every `foreign import` by a regular definition with `... = undefined`
