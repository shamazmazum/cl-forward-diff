# cl-forward-diff
[![CI](https://github.com/shamazmazum/cl-forward-diff/actions/workflows/test.yml/badge.svg)](https://github.com/shamazmazum/cl-forward-diff/actions/workflows/test.yml)

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

## Manual
**cl-forward-diff** is a Common Lisp system which provides automatic
differentiation in forward mode.

Since Common Lisp does not provide a way to define new parameteric types, all
calculations are performed for the type `dual` which is an alias for
`(sb-ext:simd-pack double-float)`. These are a constuctor and destructors for
convenience:

* `make-dual`
* `dual-realpart`
* `dual-imagpart`

The package `cl-forward-diff` provides the following mathematical functions
which you must use in the functions you want to differentiate:

* Arithmetic: `+`, `-`, `*`, `/`, `1+`, `1-`, `abs`, `signum`.
* Raise to power: `expt`, `sqrt`.
* Trigonometry: `sin`, `cos`, `tan`.
* Hyperbolic trigonometry: `sinh`, `cosh`, `tanh`.
* Exponentiation: `exp`, `log`.

These functions operate on type `ext-number` which is an abbreviation for
`(or dual real)` which means that you cannot differentiate complex functions.

Inverse trigonometric functions (`asin`, `acos`, etc.) are not yet implemented.

These functions defined or behave differently compared to their counterparts in
`cl` package:

| Function   | Difference                                                  |
|------------|-------------------------------------------------------------|
| `sqrt`     | Returns run-time error when called with negative argument.  |
| `log`      | Does not have optional argument. Also see `sqrt`.           |

Functions to de differentiated are bestly defined within a package which shadows
math functions from `cl` package with ones from `cl-forward-diff` package. See
an example:

``` lisp
(defpackage test
  (:use #:cl)
  #.(cl-forward-diff:shadowing-import-math)
  (:export #:fn #:fn2 #:fn3))
(in-package :test)

(defun fn (x)
  (1+ (* (expt (1- x) 2) 2)))

(defun fn2 (args)
  (let ((x (nth 0 args))
        (y (nth 1 args)))
    (* (1- x) (1+ y))))
    
(defun fn3 (coeffs x)
  (reduce #'+
          (snakes:generator->list
           (snakes:imap
            (lambda (c n)
              (* c (expt x n)))
            (snakes:list->generator coeffs)
            (snakes:icount 0)))))
```

You can now calculate the first derivative of `fn`. Suppose you are in `cl-user`
package. Type this in REPL:

``` lisp
CL-USER> (test:fn #d(4d0 1d0))
#<SIMD-PACK 1.9000000000000d+1 1.2000000000000d+1>
```

The returned pair contains the value of `fn` and its first derivative at the
point `4.0`. Remember that all calculations are permormed with `double-float`
values and every constant in definition of `fn` is coerced to `double-float`.
There is a helper function to calculate the first derivative:
`ad-univariate`. Check this:

``` lisp
CL-USER> (cl-forward-diff:ad-univariate #'test:fn 4)
12.0d0
```

You can calculate gradient of a function of two or more variables, like
`test:fn2`, using `ad-multivariate`.

``` lisp
CL-USER> (cl-forward-diff:ad-multivariate #'test:fn2 '(2 4))
(5.0d0 1.0d0)
```

Rather complicated functions also can be differentiated:

``` lisp
CL-USER> (cl-forward-diff:ad-univariate (alexandria:curry #'test:fn3 '(1 2 1)) 5)
12.0d0
```

## How to define piecewise functions?

Since differentiable functions must operate with dual numbers and dual numbers
do not have order, you may ask: how to define piecewise functions like the
following one?

``` lisp
(defun foo (x)
  (if (> x 2) x (* 3 x)))
```

It's easy. Just compare real part of x in the conditional form.

``` lisp
(mapcar
 (alexandria:curry
  #'cl-forward-diff:ad-univariate
  (lambda (x)
    (if (> (cl-forward-diff:dual-realpart x) 2)
        x (cl-forward-diff:* 3 x))))
 '(1 4))
```

evaluates to `(3.0d0 1.0d0)`.

## Optimization

If you want to write performat code, stick to these rules:

1. Use `(declare (optimize (speed 3)))` in functions you want to optimize.
2. Numerical arguments to differentiable functions must be of type `dual`. In
   case of using `ad-multivariate` the function must accept a list of `dual`s.
3. Differentiable functions must return one value of type `dual`. Again, you may
   wish to use toplevel declarations to make hints to the compiler, like
   `(serapeum:-> fn (dual) (values dual &optional))`.

## Global side effects
Reader macro `#D` is added to the readtable. It allows you to create dual number
literals like this `#D(3d0 1d0)` which is a shortcut for `(make-dual 3d0 1d0)`.

## Discussion (in the form of FAQ)

* *Question*: Why pass arguments to a function which is differentiated by
  `ad-multivariate` in a list rather than in a vector?  
  *Answer*: Altough you can specify a type of elements of a vector (which makes
  type handling easier), working with vectors is a bit more difficult than with
  lists. This is why I chose lists.
* *Question*: What's the difference between this system and
  [masonium/cl-autodiff](https://github.com/masonium/cl-autodiff)?  
  *Answer*: cl-autodiff works with some hard macro preprocessing and has special
  macros to define differentiable functions, like `define-with-derivatives` and
  `lambda-ad`, each of them having their limitations. `cl-forward-diff` works
  purely with functions, but maybe having a macro which converts certain symbols
  from `cl` package to `cl-forward-diff` package can be a good option.
