    Title: Writing a Simple Password Generator in Racket
    Date: 2019-03-05T16:59:10
    Thumbnail: /img/a023/thumb.png
    Tags: racket

There are many places in todays world where you need to provide a password
and, since it is not recommended to reuse them, you will need to come up with
many such passwords, all of them preferably secure.  This blog post shows how
to build a simple password generator.  You may already use a password manager
and may not have a need for a new one, still, this blog post might show you a
few useful techniques for building Racket applications.

<!-- more -->

Two things are needed to generate a secure password: a cryptographically
secure random number and a way to convert this number in a string of letters,
numbers and symbols which can be used as a password.

To generate a secure random number, we can use the Racket
`crypto-random-bytes`[racket] function, this function uses the underlying OS
facilities to generate cryptographically secure random numbers.  However, the
function returns a vector of bytes, rather than a single integer.  Since
Racket supports arbitrarily large integers, we can write a function which
converts the random bytes into a single big integer.  It is more convenient to
use the usual arithmetic operations even if the numbers involved have 512 or
1024 bits:

```racket
(define (random-bignum bits)
  (define num-bytes (exact-ceiling (/ bits 8)))
  (for/fold ([result 0])
            ([byte (in-bytes (crypto-random-bytes num-bytes))])
    (+ byte (* result 256))))
```

For simplicity, the function above will round up the number of bits to the
nearest multiple of 8, so, when asking for a 510 bit random number, it will
return a 512 bit one:

```racket
pwgen.rkt> (random-bignum 512)
2149265534656356393808323702525364123006913634758130535200642
1529460948865291665389221928490091241418209211877685483113247
50742321664506075182906096005424
pwgen.rkt> 
```

Technically, the numbers printed out by this function can be used as passwords
directly, but they are too long for practical uses. We can try printing them
out in different bases, the `~r`[racket] function allows specifying a base
when converting a number to its string representation:

```racket
pwgen.rkt> (define number (random-bignum 128))
pwgen.rkt> (for ([base '(10 16 32 36)])
             (define e (~r number #:base base))
             (printf "base ~a, length: ~a, number: ~a~%"
                      base (string-length e) e))
base 10, length: 38, number: 45179475995854230969429510001515414944
base 16, length: 32, number: 21fd40254dd38346ba70b7c04711c5a0
base 32, length: 26, number: 11vl02ajejgd3bks5no13h3hd0
base 36, length: 25, number: 20fvdccm8ijp6ulrxe7ppz928
pwgen.rkt> 
```

The higher the base, the shorter the representation of the number, this is
because base 10 uses 10 symbols (the digits '0' to '9'), but base 36 uses all
the letters plus the digits, for a total of 36 symbols.  This means that the
same random number will use 38 characters when printed in base 10, but only 25
when printed in base 36.  We could use a higher base, for example base 62, if
we allow upper and lower letters plus digits, but the `~r`[racket] function is
limited to base 36, so we need to write our own encoding function:

```racket
(define (encode-bignum bignum alphabet)
  (define base (string-length alphabet))
  (let loop ([result '()]
             [bignum bignum])
    (if (> bignum 0)
        (let-values (([q r] (quotient/remainder bignum base)))
          (let ((symbol (string-ref alphabet r)))
            (loop (cons symbol result) q)))
        (list->string result))))
```

The function takes the number to encode and an *alphabet*, which is a string
containing the valid characters to use for the encoding.  We can try it out
with base 10 and supplying the string "0123456789":

```racket
pwgen.rkt> (define number (random-bignum 128))
pwgen.rkt> number
303402022734247877297530981670554688480
pwgen.rkt> (encode-bignum number "0123456789")
"303402022734247877297530981670554688480"
pwgen.rkt> 
```

Our function is not limited to using numbers, we can use the letters a, b, c,
d, e and f to encode the same number.  Since there are only 6 symbols in our
encoding, the encoding is now longer, but it represents the same random 128
bit number:

```racket
pwgen.rkt> (encode-bignum number "abcdef")
"cbdacfaebbfdefadbfdadfcfdcbfffecbcdfeafdfddbbddabc"
```

## Creating a command-line application

We have all the building blocks we need to build a simple command line utility
to generate passwords.  To make it more interesting, the utility will accept
command line arguments to select password strength, the alphabets to use for
encoding, and will allow grouping of letters to make passwords easier to use.

We will define four alphabets for encoding the passwords: the `full-alphabet`
will use upper and lower-case letters, digits plus some symbols, this will
produce the shortest passwords for a given bit length, but it might create
passwords which are difficult to type.  The `normal-alphabet` will use upper
and lower-case letters plus digits, the `lower-case-alphabet` will use lower
case letters plus digits and a `simplified-alphabet` will use a combination of
letters and numbers which cannot be mistaken for each other (for example, it
does not contain the letter `l` or the digit `1`):

```racket
(define full-alphabet
  (string-append
   "abcdefghijklmnopqrstuvwxyz"
   "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
   "0123456789"
   "~!@#$%^&*_+-=|{}[]<>?,./"))

(define normal-alphabet
  (string-append "abcdefghijklmnopqrstuvwxyz"
                 "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
                 "0123456789"))

(define lower-case-alphabet
  (string-append "abcdefghijklmnopqrstuvwxyz" "0123456789"))

(define simplified-alphabet "acefghkrstwxyz23456789")
```

To make passwords easier to read and type, we can extend the `encode-bignum`
function to insert a separator, for example a dash (`-`), between groups of
characters to produce passwords such as "xg5z-999gh-fc8z4-czfw".  This is done
by keeping track of the current position and inserting the separator if the
current position is a multiple of the grouping:

```racket
(define (encode-bignum bignum alphabet group separator)
  (define base (string-length alphabet))
  (let loop ([position 0]
             [result '()]
             [bignum bignum])
    (if (> bignum 0)
        (let-values (([q r] (quotient/remainder bignum base)))
          (let ((symbol (string-ref alphabet r)))
            ;; NOTE: if `group` is 0, there is no grouping
            (if (and (> group 0) (> position 0) (= (remainder position group) 0))
                (loop (add1 position) (cons symbol (cons separator result)) q)
                (loop (add1 position) (cons symbol result) q))))
        (list->string (reverse result)))))
```

We also need to process the command line arguments passed in to the script to
control password length, alphabet choice and grouping; this can be done using
the `command-line`[racket] macro, but since it is a macro, it interprets its
arguments in a special way, which may be surprising if you expect a normal
function call.  See the full documentation for the macro for details, however,
below is the invocation required to accept three command line arguments `-b`
for the number of bits in the password, `-a` for the alphabet to use and `-g`
for the grouping.  Each of these arguments are optional and the syntax allows
specifying a help text for them too, so the script will print out usage
information if passed in the `-h` argument.  Each command-line option has an
argument which is a string, and the `command-line`[racket] macro allows
specifying some processing to be done on that argument.  To keep the
`command-line`[racket] call simple, the `set-bits`, `set-alphabet` and
`set-grouping` functions are provided separately, these functions validate the
argument and set the actual values for the bits, alphabet and grouping
respectively:

```racket
(require racket/cmdline)
(command-line
 #:program "pwgen"
 #:usage-help "" "pwgen is a program to generate random passwords" ""
 #:once-each
 (("-b" "--bits")
  arg
  "number of random bits in the password"
  (set-bits arg))
 
 (("-a" "--alphabet")
  arg
  ("alphabet to use for encoding"
   "must be one of: 'full', 'normal', 'lower-case' or 'simplified'")
  (set-alphabet arg))
 
 (("-g" "--group")
  arg
  "group characters of <arg> items, use 0 for no grouping"
  (set-grouping arg)))
```

For completeness, here is the definition for the `set-bits`, `set-alphabet`
and `set-grouping` functions, they check that the parameter passed in as a
string is a valid number or alphabet definition and signal an error if they
are not.  These functions also set values for the `bits`, `alphabet` and
`grouping` variables which will be used to generate the password, these
variables already have default values, so the script will still generate a
password with default parameters if some options are missing from the command
line:

```racket
(define bits 128)
(define alphabet simplified-alphabet)
(define grouping 0)

(define (set-bits arg)
  (let ((value (string->number arg)))
    (if (and (number? value) (integer? value) (> value 64))
        (set! bits value)
        (error "invalid number of bits for the password, must be at least 64"))))

(define (set-alphabet arg)
  (set! alphabet
        (cond ((string-ci=? arg "full") full-alphabet)
              ((string-ci=? arg "normal") normal-alphabet)
              ((string-ci=? arg "lower-case") lower-case-alphabet)
              ((string-ci=? arg "simplified") simplified-alphabet)
              (#t (error "unknown alphabet type")))))

(define (set-grouping arg)
  (let ((value (string->number arg)))
    (if (and (number? value) (integer? value) (> value 0))
        (set! grouping value)
        (error "invalid grouping, must be a positive integer"))))
```

All that remains is to generate the password and print it out:

```racket
(define n (random-bignum bits))
(printf "~a~%" (encode-bignum n alphabet grouping #\-)))
```

With all these updates, the script can be run from the command line, you can
also find in the entire program in this [GitHub Gist][gist].  Here is what it
looks when it runs:

```
$ racket pwgen.rkt -b 128 -g 4
tkaa-krr7-82ra-w3ks-egy3-f768-t62y-c
$ 
```

## Creating a racket package for our script

Racket packages are usually used for library functions, but our program
provides none of those -- we could export the `random-bignum` and
`encode-bignum` functions, but these are simple functions and not worth having
in a separate library.  However, Racket packages can also be used to install
additional `raco` programs.  This can be useful for us if we want to run our
script without having to remember where we stored it on disk.  To create a
package for our script we will need to:

* Create a directory, "pwgen" where the files will be placed

* Put the racket source file in that directory, the info file requires it to
  be named "pwgen.rkt" -- you can download it from this [GitHub Gist][gist] if
  you don't want to type it yourself.
  
* Add the following to an "info.rkt" file in the same directory:

```racket
#lang info
(define collection "pwgen")
(define deps '("base"))
(define pkg-desc "pwgen -- a simple password generator")
(define version "1.0")
(define pkg-authors '(alexh))
(define raco-commands '(("pwgen" (submod pwgen/pwgen main) "run pwgen" #f)))
```

* Install the package by running `raco pkg install` in the "pwgen" directory.

Note that the "pwgen.rkt" copy from the [GitHub Gist][gist] has the command
line parsing and program execution is placed inside a `module+`[racket]
section named `main`. The code inside the `main` submodule will only be
executed when the program is run from the command line using the raco command,
not when the file is required inside other files.  Note how the "info.rkt"
file has a `raco-commands` section which specifies to run the main submodule
of the "pwgen/pwgen" package for its "pwgen" command.

We can now run the pwgen utility as a `raco` command:

```
$ raco pwgen -b 128 -a full
R^go-}[PkfljGF#|B/~E
$ 
```

[gist]: https://gist.github.com/alex-hhh/c25d25ab7819a9d95a55988edd5b8a19
