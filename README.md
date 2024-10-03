# cl-forward-diff
[![CI](https://github.com/shamazmazum/cl-forward-diff/actions/workflows/test.yml/badge.svg)](https://github.com/shamazmazum/cl-forward-diff/actions/workflows/test.yml)


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

| Function | Difference                                                            |
|----------|-----------------------------------------------------------------------|
| `sqrt`   | Signals a run-time error when called with negative argument.          |
| `log`    | Does not have an optional argument. Also see `sqrt`.                  |
| `expt`   | Signals a run-time error when `cl:expt` would return a complex value. |

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
  (let ((x (aref args 0))
        (y (aref args 1)))
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

These functions also can accept real numbers:

``` lisp
CL-USER> (test:fn 4)
19
```

The returned pair contains the value of `fn` and its first derivative at the
point `4`. There is a helper function to calculate the first derivative:
`ad-univariate`. Check this:

``` lisp
CL-USER> (cl-forward-diff:ad-univariate #'test:fn 4)
12.0d0
```

You can calculate gradient of a function of two or more variables, like
`test:fn2`, using `ad-multivariate`.

``` lisp
CL-USER> (cl-forward-diff:ad-multivariate
           #'test:fn2 (cl-forward-diff:to-doubles '(2 4)))
#(5.0d0 1.0d0)
```

Rather complicated functions also can be differentiated:

``` lisp
CL-USER> (cl-forward-diff:ad-univariate (alexandria:curry #'test:fn3 '(1 2 1)) 5)
12.0d0
```

## How to define piecewise functions?

Since differentiable functions must operate on dual numbers and dual numbers
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

## Few implementation details

When the compiler cannot decide if the arguments of your functions are purely
real or dual, calls to arithmetic functions are expanded to `TWO-ARG-FOO`
counterparts. For example, if we disassemble `TEST:FN` we will see

``` lisp
CL-USER> (disassemble 'test:fn)
; disassembly for TEST:FN
; Size: 134 bytes. Origin: #x10015183E4                       ; TEST:FN
; 3E4:       498B4510         MOV RAX, [R13+16]               ; thread.binding-stack-pointer
; 3E8:       488945F8         MOV [RBP-8], RAX
; 3EC:       4883EC10         SUB RSP, 16
; 3F0:       488B55F0         MOV RDX, [RBP-16]
; 3F4:       BF02000000       MOV EDI, 2
; 3F9:       B904000000       MOV ECX, 4
; 3FE:       48892C24         MOV [RSP], RBP
; 402:       488BEC           MOV RBP, RSP
; 405:       498B45F0         MOV RAX, [R13-16]               ; thread.linkage-table
; 409:       FF90A0190100     CALL [RAX+72096]                ; CL-FORWARD-DIFF::TWO-ARG--
; 40F:       488BF2           MOV RSI, RDX
; 412:       4883EC10         SUB RSP, 16
; 416:       488BD6           MOV RDX, RSI
; 419:       488BFE           MOV RDI, RSI
; 41C:       B904000000       MOV ECX, 4
; 421:       48892C24         MOV [RSP], RBP
; 425:       488BEC           MOV RBP, RSP
; 428:       498B45F0         MOV RAX, [R13-16]               ; thread.linkage-table
; 42C:       FF90E81B0100     CALL [RAX+72680]                ; CL-FORWARD-DIFF::TWO-ARG-*
; 432:       4883EC10         SUB RSP, 16
; 436:       BF04000000       MOV EDI, 4
; 43B:       B904000000       MOV ECX, 4
; 440:       48892C24         MOV [RSP], RBP
; 444:       488BEC           MOV RBP, RSP
; 447:       498B45F0         MOV RAX, [R13-16]               ; thread.linkage-table
; 44B:       FF90E81B0100     CALL [RAX+72680]                ; CL-FORWARD-DIFF::TWO-ARG-*
; 451:       BF02000000       MOV EDI, 2
; 456:       B904000000       MOV ECX, 4
; 45B:       FF7508           PUSH QWORD PTR [RBP+8]
; 45E:       498B45F0         MOV RAX, [R13-16]               ; thread.linkage-table
; 462:       FFA0A8190100     JMP [RAX+72104]                 ; CL-FORWARD-DIFF::TWO-ARG-+
; 468:       CC0F             INT3 15                         ; Invalid argument count trap
NIL
```

In this case SBCL has a limited ability to derive type of your function. E.g. if
you inline `TEST:FN` and write the following code:

``` lisp
(sera:-> bad-fn ((or diff:dual fixnum))
         (values single-float &optional))
(defun bad-fn (x)
  (fn x))
```

you will get:

``` lisp
; in: DEFUN BAD-FN
;     (SB-INT:NAMED-LAMBDA TEST::BAD-FN
;         (TEST::X)
;       (BLOCK TEST::BAD-FN (TEST:FN TEST::X)))
; 
; caught WARNING:
;   Derived type of ((CL-FORWARD-DIFF::TWO-ARG-+ CL-FORWARD-DIFF::X 1)) is
;     (VALUES (OR INTEGER (SB-EXT:SIMD-PACK DOUBLE-FLOAT)) &OPTIONAL),
;   conflicting with the declared function return type
;     (VALUES SINGLE-FLOAT &OPTIONAL).
;   See also:
;     The SBCL Manual, Node "Handling of Types"
; 
; compilation unit finished
;   caught 1 WARNING condition
WARNING: redefining TEST::BAD-FN in DEFUN
```

If the compiler known that the arguments are either strictly dual or strictly
real, it will either open-code mathematical functions using SIMD instructions or
use functions from the `CL` package. For example:

``` lisp
(sera:-> good-1 (real)
         (values real &optional))
(defun good-1 (x)
  (fn x))

(disassemble 'good-1)
```

gives

``` lisp
; disassembly for TEST::GOOD-1
; Size: 66 bytes. Origin: #x1001519330                        ; TEST::GOOD-1
; 30:       498B4510         MOV RAX, [R13+16]                ; thread.binding-stack-pointer
; 34:       488945F8         MOV [RBP-8], RAX
; 38:       488B55F0         MOV RDX, [RBP-16]
; 3C:       BF02000000       MOV EDI, 2
; 41:       FF142540050020   CALL [#x20000540]                ; #x1000000EA0: GENERIC--
; 48:       488BF2           MOV RSI, RDX
; 4B:       488BFE           MOV RDI, RSI
; 4E:       FF142548050020   CALL [#x20000548]                ; #x1000000F10: GENERIC-*
; 55:       BF04000000       MOV EDI, 4
; 5A:       FF142548050020   CALL [#x20000548]                ; #x1000000F10: GENERIC-*
; 61:       BF02000000       MOV EDI, 2
; 66:       FF142538050020   CALL [#x20000538]                ; #x1000000E30: GENERIC-+
; 6D:       C9               LEAVE
; 6E:       F8               CLC
; 6F:       C3               RET
; 70:       CC0F             INT3 15                          ; Invalid argument
count trap
```

and

``` lisp
(sera:-> good-2 (diff:dual)
         (values diff:dual &optional))
(defun good-2 (x)
  (fn x))

(disassemble 'good-2)
```

produces

``` lisp
; disassembly for TEST::GOOD-2
; Size: 178 bytes. Origin: #x1001519401                       ; TEST::GOOD-2
; 01:       498B4510         MOV RAX, [R13+16]                ; thread.binding-stack-pointer
; 05:       488945F8         MOV [RBP-8], RAX
; 09:       0F2805A0FFFFFF   MOVAPS XMM0, [RIP-96]            ; [#x10015193B0]
; 10:       C5C95CC0         VSUBPD XMM0, XMM6, XMM0
; 14:       C5E957D2         VXORPD XMM2, XMM2, XMM2
; 18:       F20F10D0         MOVSD XMM2, XMM0
; 1C:       C4E37905C801     VPERMILPD XMM1, XMM0, 1
; 22:       C5D957E4         VXORPD XMM4, XMM4, XMM4
; 26:       F20F10E1         MOVSD XMM4, XMM1
; 2A:       C5F157C9         VXORPD XMM1, XMM1, XMM1
; 2E:       F20F10C8         MOVSD XMM1, XMM0
; 32:       C4E37905C001     VPERMILPD XMM0, XMM0, 1
; 38:       C5D157ED         VXORPD XMM5, XMM5, XMM5
; 3C:       F20F10E8         MOVSD XMM5, XMM0
; 40:       660F28DA         MOVAPD XMM3, XMM2
; 44:       F20F59D9         MULSD XMM3, XMM1
; 48:       F20F59D5         MULSD XMM2, XMM5
; 4C:       F20F59CC         MULSD XMM1, XMM4
; 50:       F20F58CA         ADDSD XMM1, XMM2
; 54:       C5F910C3         VMOVUPD XMM0, XMM3
; 58:       C5F914C1         VUNPCKLPD XMM0, XMM0, XMM1
; 5C:       C5F958C0         VADDPD XMM0, XMM0, XMM0
; 60:       0F280D49FFFFFF   MOVAPS XMM1, [RIP-183]           ; [#x10015193B0]
; 67:       C5F958C1         VADDPD XMM0, XMM0, XMM1
; 6B:       4D896D28         MOV [R13+40], R13                ; thread.pseudo-atomic-bits
; 6F:       498B5570         MOV RDX, [R13+112]               ; thread.mixed-tlab
; 73:       4883C220         ADD RDX, 32
; 77:       493B5578         CMP RDX, [R13+120]
; 7B:       7727             JA L2
; 7D:       49895570         MOV [R13+112], RDX               ; thread.mixed-tlab
; 81:       4883C2EF         ADD RDX, -17
; 85: L0:   66C742F16503     MOV WORD PTR [RDX-15], 869
; 8B:       4D316D28         XOR [R13+40], R13                ; thread.pseudo-atomic-bits
; 8F:       7402             JEQ L1
; 91:       CC09             INT3 9                           ; pending interrupt trap
; 93: L1:   48C742F902000000 MOV QWORD PTR [RDX-7], 2
; 9B:       0F294201         MOVAPS [RDX+1], XMM0
; 9F:       C9               LEAVE
; A0:       F8               CLC
; A1:       C3               RET
; A2:       CC0F             INT3 15                          ; Invalid argument count trap
; A4: L2:   6A20             PUSH 32
; A6:       FF142578040020   CALL [#x20000478]                ; #x10000004F0: ALLOC-TRAMP
; AD:       5A               POP RDX
; AE:       80CA0F           OR DL, 15
; B1:       EBD2             JMP L0
```

The reason I chose a packed SIMD representation for duals over usual structures
is that there is no consing for intermediate results.

## Global side effects
Reader macro `#D` is added to the readtable. It allows you to create dual number
literals like this `#D(3d0 1d0)` which is a shortcut for `(make-dual 3d0 1d0)`.

## Discussion (in the form of FAQ)

* *Question*: What's the difference between this system and
  [masonium/cl-autodiff](https://github.com/masonium/cl-autodiff)?  
  *Answer*: cl-autodiff works with some hard macro preprocessing and has special
  macros to define differentiable functions, like `define-with-derivatives` and
  `lambda-ad`, each of them having their limitations. `cl-forward-diff` works
  purely with functions, but maybe having a macro which converts certain symbols
  from `cl` package to `cl-forward-diff` package can be a good option.
* *Question*: What's the difference between this system and using Coalton?  
  *Answer*: There is the `Dual` type in `coalton-library/math` which serves the
  same purpose. But I simply don't know much about Coalton and Coalton — Common
  Lisp interop.
