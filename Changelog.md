# Changelog

## New in version 0.4

* Do not arguments to `dual` if it is not required. You can now call these
  functions with `real` values and get `real`.

## New in version 0.3

* `ad-multivariate` now accepts a simple one-dimensional array of doubles as its
  second argument. The first argument must be of type `(-> ((simple-array dual
  (*))) (values dual &optional))` (i.e. lists were replaced with vectors).
* New helper function `to-doubles` which converts a sequence of real numbers to
  a simple array of double floats.

## New in version 0.2

This section can be skipped if this is the first time you use
**cl-forward-diff**.

* Dual numbers are now stored in XMM registers whenever possible. This requires
  SBCL > 2.2.6.
* Now components of dual numbers are floats with double precision.
* Dual numbers are printed as unreadable objects, but you still can use #D macro
  for literals.
* Tests for (in)equality (`/=`, `=`) are removed. I did not found any use cases
  for them anyway.
