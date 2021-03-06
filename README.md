# cl-forward-diff
[![CI](https://github.com/shamazmazum/cl-forward-diff/actions/workflows/test.yml/badge.svg)](https://github.com/shamazmazum/cl-forward-diff/actions/workflows/test.yml)

## Manual
**cl-forward-diff** is a Common Lisp system which provides automatic
differentiation in forward mode.

Since Common Lisp does not provide a way to define new parameteric types, all
calculations are performed for the type `dual` which is a
structure with two `single-float`s inside:

~~~~{.lisp}
(defstruct dual
  (realpart 0f0 :type single-float)
  (imagpart 0f0 :type single-float))
~~~~

The package `cl-forward-diff` provides the following mathematical functions
which you must use in the functions you want to differentiate:

* Equality: `=`, `/=`.
* Arithmetic: `+`, `-`, `*`, `/`, `1+`, `1-`, `abs`, `signum`.
* Raise-to-power: `expt`, `sqrt`.
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

~~~~{.lisp}
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
~~~~

You can now calculate the first derivative of `fn`. Suppose you are in `cl-user`
package. Type this in REPL:

~~~~
CL-USER> (test:fn #d(4.0 1.0))
#D(19.0 12.0)
~~~~

The returned pair contains the value of `fn` and its first derivative at the
point `4.0`. Remember that all calculations are permormed with `single-float`
values and every constant in definition of `fn` is coerced to `single-float`.
There is a helper function to calculate the first derivative:
`ad-univariate`. Check this:

~~~~
CL-USER> (cl-forward-diff:ad-univariate #'test:fn 4)
12.0
~~~~

You can calculate gradient of a function of two or more variables, like
`test:fn2`, using `ad-multivariate`.

~~~~
CL-USER> (cl-forward-diff:ad-multivariate #'test:fn2 '(2 4))
(5.0 1.0)
~~~~

Rather complicated functions also can be differentiated:

~~~~
CL-USER> (cl-forward-diff:ad-univariate (alexandria:curry #'test:fn3 '(1.0 2.0 1.0)) 5.0)
12.0
~~~~

## How to define piecewise functions?

Since differentiable functions must operate with dual numbers and dual numbers
do not have order, you may ask: how to define piecewise functions like the
following one?

~~~~{.lisp}
(defun foo (x)
  (if (> x 2) x (* 3 x)))
~~~~

It's easy. Just compare real part of x in the conditional form.

~~~~{.lisp}
(mapcar
 (alexandria:curry
  #'cl-forward-diff:ad-univariate
  (lambda (x)
    (if (> (cl-forward-diff:dual-realpart x) 2)
        x (cl-forward-diff:* 3 x))))
 '(1 4))
~~~~

evaluates to `(3.0 1.0)`.

## Optimization

Now, lets see the assembly code for `test:fn` (aquired with SBCL 2.1.11):

~~~~
CL-USER> (disassemble 'test:fn)
; disassembly for TEST:FN
; Size: 120 bytes. Origin: #x229C6D41                         ; TEST:FN
; 41:       498B4510         MOV RAX, [R13+16]                ; thread.binding-stack-pointer
; 45:       488945F8         MOV [RBP-8], RAX
; 49:       4883EC10         SUB RSP, 16
; 4D:       488B55F0         MOV RDX, [RBP-16]
; 51:       B902000000       MOV ECX, 2
; 56:       48892C24         MOV [RSP], RBP
; 5A:       488BEC           MOV RBP, RSP
; 5D:       B8428C4320       MOV EAX, #x20438C42              ; #<FDEFN CL-FORWARD-DIFF:1->
; 62:       FFD0             CALL RAX
; 64:       480F42E3         CMOVB RSP, RBX
; 68:       4883EC10         SUB RSP, 16
; 6C:       BF04000000       MOV EDI, 4
; 71:       B904000000       MOV ECX, 4
; 76:       48892C24         MOV [RSP], RBP
; 7A:       488BEC           MOV RBP, RSP
; 7D:       B8E28B4320       MOV EAX, #x20438BE2              ; #<FDEFN CL-FORWARD-DIFF:EXPT>
; 82:       FFD0             CALL RAX
; 84:       480F42E3         CMOVB RSP, RBX
; 88:       4883EC10         SUB RSP, 16
; 8C:       BF04000000       MOV EDI, 4
; 91:       B904000000       MOV ECX, 4
; 96:       48892C24         MOV [RSP], RBP
; 9A:       488BEC           MOV RBP, RSP
; 9D:       B8A2894320       MOV EAX, #x204389A2              ; #<FDEFN CL-FORWARD-DIFF:*>
; A2:       FFD0             CALL RAX
; A4:       480F42E3         CMOVB RSP, RBX
; A8:       B902000000       MOV ECX, 2
; AD:       FF7508           PUSH QWORD PTR [RBP+8]
; B0:       B8828C4320       MOV EAX, #x20438C82              ; #<FDEFN CL-FORWARD-DIFF:1+>
; B5:       FFE0             JMP RAX
; B7:       CC10             INT3 16                          ; Invalid argument count trap
NIL
~~~~

Not very impressive, is it? Now let's change our code in the following way:

~~~~{.lisp}
(defpackage test
  (:use #:cl)
  #.(cl-forward-diff:shadowing-import-math)
  (:export #:fn))
(in-package :test)

(declaim (optimize (speed 3)))

(defun fn (x)
  (declare (type dual x))
  (1+ (* (expt (1- x) 2) 2)))

(defun fn2 (args)
  (let ((x (nth 0 args))
        (y (nth 1 args)))
    (declare (type dual x y))
    (* (1- x) (1+ y))))
~~~~

Now the disassembly looks like this:

~~~~
CL-USER> (disassemble 'test:fn)
; disassembly for TEST:FN
; Size: 340 bytes. Origin: #x229D8A98                         ; TEST:FN
; A98:       F30F104A05       MOVSS XMM1, [RDX+5]
; A9D:       F30F10520D       MOVSS XMM2, [RDX+13]
; AA2:       F30F5C0DAEFFFFFF SUBSS XMM1, [RIP-82]            ; [#x229D8A58]
; AAA:       4883EC10         SUB RSP, 16
; AAE:       660F7ED7         MOVD EDI, XMM2
; AB2:       48C1E720         SHL RDI, 32
; AB6:       4080CF19         OR DIL, 25
; ABA:       660F7ECA         MOVD EDX, XMM1
; ABE:       48C1E220         SHL RDX, 32
; AC2:       80CA19           OR DL, 25
; AC5:       B904000000       MOV ECX, 4
; ACA:       48892C24         MOV [RSP], RBP
; ACE:       488BEC           MOV RBP, RSP
; AD1:       B842874320       MOV EAX, #x20438742             ; #<FDEFN CL-FORWARD-DIFF:MAKE-DUAL>
; AD6:       FFD0             CALL RAX
; AD8:       F30F104A05       MOVSS XMM1, [RDX+5]
; ADD:       F30F10620D       MOVSS XMM4, [RDX+13]
; AE2:       F30F105205       MOVSS XMM2, [RDX+5]
; AE7:       F30F105A0D       MOVSS XMM3, [RDX+13]
; AEC:       0F28E9           MOVAPS XMM5, XMM1
; AEF:       F30F59EA         MULSS XMM5, XMM2
; AF3:       F30F59D9         MULSS XMM3, XMM1
; AF7:       F30F59E2         MULSS XMM4, XMM2
; AFB:       F30F58E3         ADDSS XMM4, XMM3
; AFF:       4883EC10         SUB RSP, 16
; B03:       660F7EE7         MOVD EDI, XMM4
; B07:       48C1E720         SHL RDI, 32
; B0B:       4080CF19         OR DIL, 25
; B0F:       660F7EEA         MOVD EDX, XMM5
; B13:       48C1E220         SHL RDX, 32
; B17:       80CA19           OR DL, 25
; B1A:       B904000000       MOV ECX, 4
; B1F:       48892C24         MOV [RSP], RBP
; B23:       488BEC           MOV RBP, RSP
; B26:       B842874320       MOV EAX, #x20438742             ; #<FDEFN CL-FORWARD-DIFF:MAKE-DUAL>
; B2B:       FFD0             CALL RAX
; B2D:       488BF2           MOV RSI, RDX
; B30:       488975F8         MOV [RBP-8], RSI
; B34:       4883EC10         SUB RSP, 16
; B38:       488B1509FFFFFF   MOV RDX, [RIP-247]              ; 2.0
; B3F:       488B3DFAFEFFFF   MOV RDI, [RIP-262]              ; 0.0
; B46:       B904000000       MOV ECX, 4
; B4B:       48892C24         MOV [RSP], RBP
; B4F:       488BEC           MOV RBP, RSP
; B52:       B842874320       MOV EAX, #x20438742             ; #<FDEFN CL-FORWARD-DIFF:MAKE-DUAL>
; B57:       FFD0             CALL RAX
; B59:       488B75F8         MOV RSI, [RBP-8]
; B5D:       F30F104E05       MOVSS XMM1, [RSI+5]
; B62:       F30F10660D       MOVSS XMM4, [RSI+13]
; B67:       F30F105205       MOVSS XMM2, [RDX+5]
; B6C:       F30F105A0D       MOVSS XMM3, [RDX+13]
; B71:       0F28E9           MOVAPS XMM5, XMM1
; B74:       F30F59EA         MULSS XMM5, XMM2
; B78:       F30F59D9         MULSS XMM3, XMM1
; B7C:       F30F59E2         MULSS XMM4, XMM2
; B80:       F30F58E3         ADDSS XMM4, XMM3
; B84:       4883EC10         SUB RSP, 16
; B88:       660F7EE7         MOVD EDI, XMM4
; B8C:       48C1E720         SHL RDI, 32
; B90:       4080CF19         OR DIL, 25
; B94:       660F7EEA         MOVD EDX, XMM5
; B98:       48C1E220         SHL RDX, 32
; B9C:       80CA19           OR DL, 25
; B9F:       B904000000       MOV ECX, 4
; BA4:       48892C24         MOV [RSP], RBP
; BA8:       488BEC           MOV RBP, RSP
; BAB:       B842874320       MOV EAX, #x20438742             ; #<FDEFN CL-FORWARD-DIFF:MAKE-DUAL>
; BB0:       FFD0             CALL RAX
; BB2:       F30F104A05       MOVSS XMM1, [RDX+5]
; BB7:       F30F10520D       MOVSS XMM2, [RDX+13]
; BBC:       F30F580D94FEFFFF ADDSS XMM1, [RIP-364]           ; [#x229D8A58]
; BC4:       660F7ECA         MOVD EDX, XMM1
; BC8:       48C1E220         SHL RDX, 32
; BCC:       80CA19           OR DL, 25
; BCF:       660F7ED7         MOVD EDI, XMM2
; BD3:       48C1E720         SHL RDI, 32
; BD7:       4080CF19         OR DIL, 25
; BDB:       B904000000       MOV ECX, 4
; BE0:       FF7508           PUSH QWORD PTR [RBP+8]
; BE3:       B842874320       MOV EAX, #x20438742             ; #<FDEFN CL-FORWARD-DIFF:MAKE-DUAL>
; BE8:       FFE0             JMP RAX
; BEA:       CC10             INT3 16                         ; Invalid argument count trap
NIL
~~~~

You can see that all math is optimized. There are only a constructor for `dual`
type which is left in abundance, but this one is hard to optimize away. Anyway,
execution time of optimized function is considerably faster.

So there are a general rules for optimization:

1. Use `(declare (optimize (speed 3)))` in functions you want to optimize.
2. Numerical arguments to differentiable functions must be of type `dual`. If
   the compiler cannot infer the type, use declarations like one you saw in
   `test:fn`. In case of using `ad-multivariate` the function must accept a list
   of `dual`s.
3. Differentiable functions must return one value of type `dual`. Again, you may
   wish to use toplevel declarations to make hints to the compiler, like
   `(serapeum-> fn (dual) (values dual &optional))`.

## Global side effects
Reader macro `#D` is added to the readtable. It allows you to create dual number
literals like this `#D(3.0 1.0)` which is a shortcut for `(make-dual 3.0 1.0)`.

On SBCL, when feature `:single-float-tran` is present IR1 transformations are
added to some of math functions (e.g. trigonometric functions) which produce the
code that uses `sinf`, `cosf` etc. instead of double float versions `sin`, `cos`
etc. See
[sbcl-single-float-tran](https://github.com/shamazmazum/sbcl-single-float-tran)
for more information.


## TODO

* Implement better storage for dual numbers (if possible at all). Dual numbers
  must have their own type, so `(cons single-float)` will not do. Also it would
  be nice to make `make-dual` foldable.
* ~~Implement more math functions (`sinh`, `cosh`, `tanh` etc.)~~ Partially
  done.
* ~~Implement `=` for number of arguemnts > 2.~~ Done
* ~~Move SBCL transforms to another system, as this is not related to AD.~~
  Done.
* Maybe optimize `=`, `/=`, `+`, `-`, `*`, `/` for number of arguments > 2?

## Discussion (in the form of FAQ)

* *Question*: why choose single floats over double floats?  
  *Answer*: When using `double-float` type in optimized function you usually get
  a weird compiler note, like this one:
  `note: doing float to pointer coercion (cost 13) to "<return value>"`. I don't
  quite understand why this happens (maybe SBCL stores a tag in one half of XMM
  register), but too many of this messages make useful notes unreadable.
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
  purely with (polymorphic) functions, but maybe having a macro which converts
  certain symbols from `cl` package to `cl-forward-diff` package can be a good
  option.
