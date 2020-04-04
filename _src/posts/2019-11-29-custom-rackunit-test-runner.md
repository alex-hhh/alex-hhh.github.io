    Title: Custom Rackunit Test Runner
    Date: 2019-11-29T08:31:21
    Thumbnail: /img/a033/thumb.png
    Tags: racket

As the number of tests for a program continues to increase, it is no longer
sufficient to know that all tests passed or which test failed.  Other
questions start to become important: were some tests inadvertently disabled?
which tests were skipped? how did the duration of a test evolve over several
versions of a program?  Existing build platforms, such as Azure Devops, can
help with test management, but in order make use of their features, we need to
extend the Racket unit test library to report the test results to the build
pipeline.

<!-- more -->

In this blog post we'll look at how to provide a custom test runner for the
Racket [rackunit][rackunit] package which allows reporting test results to
build pipelines.  While in my particular case, I used this to integrate with
Azure Devops, the results are written in a [JUnit][junit-schema] compatible
format, which is read and understood by many other platforms.

**If you are in a hurry**, the full implementation of the custom test runner
is available [here][ctr], and the tests in the same location make use of all
of its features.  For an example of publishing test results to Azure Devops,
see the [azure-common.yml][az-common] file.

![Racket Reports in Azure Pipelines](/img/a033/azure-test-report.png)

## Rackunit Overview

The [rackunit][rackunit] package ships with the standard Racket installation
and allows writing unit tests for Racket packages and applications.  At its
most basic level, one can simply use the various [check functions][ru-checks]
inside `test` submodules and run the tests with a `raco test` command.  A
failed test will report the condition that triggered the failure an the source
location where it happened, while the `raco test` command will return a
non-zero exit code when tests fail -- this last part is important if tests are
incorporated into build pipelines.

As a program and its test suite grows, writing checks directly inside test
submodules becomes less attractive, since the test run will stop at the first
failed test.  [rackunit][rackunit] provides two additional constructs for
organizing test code, and these help manage the complexities of a growing test
suite.  A [test-case][test-case] allows grouping a set of checks and, while a
test case will abort if one of its checks fail, other test cases will still
run.  A [test-suite][test-suite] in turn is a collection of test cases.
Unlike a test case, which will run immediately where it is defined, defining a
test suite will only produce a test suite object which will need to be run
explicitly.

The example code below illustrates how test cases and test suites work.  Test
suites can be nested, as shown where the `inner-test-suite` is referenced from
`a-test-suite`.  Also a test can either succeed, fail when a check condition
is not met, or report an error -- this last part is illustrated in test cases
3 and C where the test calls `error`:

```racket
#lang racket
(require rackunit)

(define inner-test-suite
  (test-suite "inner-test-suite"
   (test-case "test case 1" (check-true #t))
   (test-case "test case 2" (check-true #f))
   (test-case "test case 3" (error "user error"))))

(define a-test-suite
  (test-suite "a-test-suite"
   inner-test-suite
   (test-case "test case A" (check-true #t))
   (test-case "test case B" (check-true #f))
   (test-case "test case C" (error "user error"))))
```

Evaluating the code above in Racket will not produce any output, since the
test suites are only defined, but not run.  To actually run these tests, we
need to use a test runner, and rackunit provides two of them.  The textual
interface will run the tests and output any error information to the console:

```racket
(require  rackunit/text-ui)
(run-tests a-test-suite 'verbose)
```

Running it produces the output below.  Note how all the test failures are
reported (two failures and two errors), and the test run does not stop at the
first failure:

```
--------------------
inner-test-suite > a-test-suite > test case 2
; FAILURE
; custom-runner-0a.rkt:7:28
name:        check-true
location:    custom-runner-0a.rkt:7:28
expression:  (check-true #f)
params:      '(#f)
--------------------
inner-test-suite > a-test-suite > test case 3
; ERROR
user error
--------------------
a-test-suite > test case B
; FAILURE
; custom-runner-0a.rkt:14:28
name:        check-true
location:    custom-runner-0a.rkt:14:28
expression:  (check-true #f)
params:      '(#f)
--------------------
a-test-suite > test case C
; ERROR
user error
--------------------
2 success(es) 2 failure(s) 2 error(s) 6 test(s) run
4
```

The second test runner provides a GUI interface and it is available from the
`rackunit/gui` module and Running it will open a window showing the progress
of test execution along with detailed information about each test case:

```racket
(require rackunit/gui)
(test/gui a-test-suite #:wait? #t)
```

![DrRacket GUI Test Runner](/img/a033/rackunit-gui-runner.png)

### What about "raco test" and the `test` submodules?

The examples so far are plain Racket programs and the tests are evaluated when
the program is run.  Since it is not useful to run the tests every time the
program is run, test code would have to be placed in separate files.  In
Racked, one can mix tests and regular code in the same file and this is done
by placing the tests inside `test` submodules, like so:

```racket
(module+ test
  ;; add test case and test suite definitions here
  (require rackunit/gui)
  (test/gui a-test-suite #:wait? #t))
```

The test submodules in the source files are ignored when Racket compiles and
runs a program, so there is no runtime cost of placing them along with the
source.  The "raco test" command can be used to run these submodules, and
therefore the tests themselves.

## A Custom Test Runner

The rackunit text interface is useful for running tests from the command line,
which in turn makes it useful for placing it into build pipelines so test are
automatically run.  Unfortunately, it only provides detailed information about
failures: a successful run will only report the number of tests that were run.
The GUI interface provides detailed information about all the tests that are
run, including successful ones, but it is not suitable for use in automated
build pipelines.  If we want a test runner which can both be run from the
command line and reports detailed results, we must build our own.
Fortunately, [rackunit][rackunit] provides the necessary extension mechanisms.

### The Basics

The [foldts-test-suite][foldts] function is part of the rackunit library and
can be used to run the test cases inside a test suite, while allowing
customization on how the test run.  The function is passed in the test suite
to run and three functions: one which will be called before a test suite
beings to execute, one called after a test suite completes and a function to
run the test case itself.  The last function is important because it allows
the user to control whether a test case runs or not.

The full details for [foldts-test-suite][foldts] are available in the
documentation, but for our purposes, we'll write a wrapper function which uses
a collector object to report and control how tests are run.  Each callback
function simply delegate the call to this collector object:

```racket
(define (fold-test-results collector test-suite)
  (foldts-test-suite
   ;; Called before a test suite is run
   (lambda (suite name before after seed)
     (before)
     (send collector on-test-suite-start name))
   ;; Called after a test suite completed
   (lambda (suite name before after seed kid-seed)
     (after)
     (send collector on-test-suite-completed name))
   ;; Called once for each test case in a suite
   (lambda (case name action seed)
     (when (send collector before-test-case-start name)
       (define result (run-test-case name action))
       (send collector on-test-case-completed result)))
   ;; The seed (unused by our code)
   (void)
   ;; The test suite we run
   test-suite))
```

The collector class provides four methods that are used to report the progress
of running the test suite.  The `before-test-case-start` method is somewhat
special: it returns `#t` if the test case should be run and `#f` if it should
be skipped.  For now this method always returns `#t`, but we'll use this to
control which tests are run and which are skipped.  Other than that, the
methods simply print the progress of the test:

```racket
(define test-result-collector%
  (class object% (init) (super-new)
    (define/public (on-test-suite-start name)
      (printf "*** Testsuite ~a~%" name))
    (define/public (on-test-suite-completed name)
      (printf "*** Testsuite ~a completed~%" name))
    (define/public (before-test-case-start name)
      (printf "\t~a: " name)
      #t)
    (define/public (on-test-case-completed result)
      (printf "~a~%" (test-result-tag result)))))
```

The `on-test-case-completed` method also receives the result of running the
test, as a `test-result` structure (defined in the rackunit library).
rackunit will supply different structure instances for each result type
(success, failure and error), and this makes it somewhat difficult to just
print out a simple result tag.  The `test-result-tag` function helps with
this:

```racket
(define (test-result-tag result)
  (cond
    ((test-failure? result) 'fail)
    ((test-success? result) 'ok)
    ((test-error? result) 'error)
    (#t 'unknown)))
```

Finally, we can write our own `run-tests` function to run the test suite and
running it on the sample test suite will produce the output below:

```racket
(define (run-tests test-suite)
  (define collector (new test-result-collector%))
  (fold-test-results collector test-suite))
(run-tests a-test-suite)
```

Our test runner now reports each test case as it runs, but even though several
tests fail, running the program using raco test will still return a 0 exit
code, indicating success. We'll need to address this next.

```
*** Testsuite a-test-suite
*** Testsuite inner-test-suite
	test case 1: ok
	test case 2: fail
	test case 3: error
*** Testsuite inner-test-suite completed
	test case A: ok
	test case B: fail
	test case C: error
*** Testsuite a-test-suite completed
```

### Non-zero exit code: report failures to "raco test"

The `raco test` command will monitor the test logger for any test failures, so
to inform it of the test status, we need to update the
`on-test-case-completed` method to use `test-log!` to report the status of a
test:

```racket
(require rackunit/log) ;; for test-log!

(define test-result-collector%
  (class object% (init) (super-new)
    ;; other methods are unchanged
    (define/public (on-test-case-completed result)
      (printf "~a~%" (test-result-tag result))
      ;; Report the result to raco test
      (test-log! (equal? (test-result-tag result) 'ok)))
    ))
```

Running the program using `raco test` will now result in an exit code of 1,
since tests failed.  While still very basic, this test runner can now be used
in build pipelines and the build will fail when tests fail:

```
$ raco test custom-runner-2.rkt
raco test: "custom-runner-2.rkt"
*** Testsuite outer-test-suite
*** Testsuite inner-test-suite
	test case 1: ok
	test case 2: fail
	test case 3: error
*** Testsuite inner-test-suite completed
	test case A: ok
	test case B: fail
	test case C: error
*** Testsuite outer-test-suite completed
4/6 test failures
alexh@ALEX-S2 ~/Projects/Racket/junit-runner
$ echo $?
1
```

### Detailed Failure Information

The rackunit text runner will report detailed information when a test fails,
and it would be nice if our test runner did the same, as it is very hard to
determine what went wrong when only "fail" or "error" is reported.  The
`test-result`[racket] that is passed to `on-test-case-completed` contains
detailed information about the failure or error, but it is in the form of
nested `check-info`[racket] structures, which is difficult to use.
Fortunately, the `display-test-failure/error` can be used to print out the
result to the `current-error-port`[racket].  We can write a wrapper around
`display-test-failure/error` to only display this information when it is
needed:

```racket
(define (maybe-display-result result)
  (define name (test-result-test-case-name result))
  (define value (cond ((test-failure? result)
                       (test-failure-result result))
                      ((test-error? result)
                       (test-error-result result))
                      (#t #f)))
  (when value
    (display-test-failure/error value name)))
```

... and we'll need to update the `on-test-case-completed` to use this wrapper
function:

```racket
(define test-result-collector%
  (class object% (init) (super-new)
    ;; other methods are unchanged
    (define/public (on-test-case-completed result)
      (printf "~a~%" (test-result-tag result))
      ;; Maybe display failure information
      (maybe-display-result result)
      (test-log! (equal? (test-result-tag result) 'ok)))
    ))
```

Another small detail is that `display-test-failure/error` will print out to
the `current-error-port`[racket] not the `current-output-port`[racket], so by
default, the errors will not be interleaved correctly with the progress
report.  To fix that, we can make the two output ports the same in our version
of `run-tests`:

```racket
(define (run-tests test-suite)
  (parameterize ([current-output-port (current-error-port)])
    (define collector (new test-result-collector%))
    (fold-test-results collector test-suite)))
```

Finally, we have the same level of information that the default text runner
provides, plus additional reports about the successful test cases:

```
$ raco test custom-runner-3.rkt
raco test: "custom-runner-3.rkt"
*** Testsuite outer-test-suite
*** Testsuite inner-test-suite
	test case 1: ok
	test case 2: fail
--------------------
test case 2
FAILURE
name:       check-true
location:   custom-runner-3.rkt:80:28
params:     '(#f)
--------------------
	test case 3: error
--------------------
test case 3
ERROR
user error
--------------------
*** Testsuite inner-test-suite completed
	test case A: ok
	test case B: fail
--------------------
test case B
FAILURE
name:       check-true
location:   custom-runner-3.rkt:91:28
params:     '(#f)
--------------------
	test case C: error
--------------------
test case C
ERROR
user error
--------------------
*** Testsuite outer-test-suite completed
4/6 test failures
```

So far, our test runner provides little more information when compared to the
one supplied by rackunit.  While the text output can be enhanced in various
ways, such as displaying test durations, the real usefulness comes when tests
are reported in formats that other systems understand, and the first real step
towards that goal is to collect the test results.

### Test Duration

An JUnit test report requires the test duration to be reported for each test
case, so before we look at collecting test results, let's measure and display
the duration of our tests.  The easiest way to measure the duration of the
test is inside `fold-test-results`, where the test is run, than report it to
the `on-test-case-completed` method:

```racket
(define (fold-test-results test-suite collector)
  (foldts-test-suite
   ;; Called before a test suite is run
   (lambda (suite name before after seed)
     (before)
     (send collector on-test-suite-start name))
   ;; Called after a test suite completed
   (lambda (suite name before after seed kid-seed)
     (after)
     (send collector on-test-suite-completed name))
   ;; Called once for each test case in a suite
   (lambda (case name action seed)
     (when (send collector before-test-case-start name)
       (define start (current-inexact-milliseconds))
       (define result (run-test-case name action))
       (define end (current-inexact-milliseconds))
       (send collector on-test-case-completed result (- end start))))
   ;; The seed (unused by our code)
   (void)
   ;; The test suite we run
   test-suite))
```

For now, the `on-test-case-completed` method will simply display the test
duration along with the result status:

```racket
(define test-result-collector%
  (class object% (init) (super-new)
    ;; Other methods are unchanged...
    (define/public (on-test-case-completed result duration)
      ;; Display the result of running this test
      (printf "~a (~a ms)~%" (test-result-tag result) (~r duration #:precision 2))
      (maybe-display-result result)
      (test-log! (equal? (test-result-tag result) 'ok)))
  ))
```

### Collect Test Results

The methods of the `test-result-collector%` class can also collect data about
the tests that re run, in addition to printing the results.  Rather than
storing the data as lists-of-lists, it is useful to define some structures and
helper functions: the `tcr` structure holds the result and duration of a test
case.  Since the name of the test case is in the result object itself, it is
not stored directly in the `tcr` structure.  A `tsr` structure holds
information about the test suite and associated test cases: start time, total
duration, number of tests and how many errors there were, plus a list of test
case results.  Since the `tsr` structure has lots of members, which are all
either 0 or empty when a new test suite is run, the `fresh-tsr` function helps
create a new `tsr` instance.

```racket
(struct tcr (result duration) #:transparent)

(struct tsr (name timestamp duration total failures errors tcrs) #:transparent)

(define (fresh-tsr name)
  (tsr name (current-inexact-milliseconds) 0 0 0 0 '()))
```

The structures, as we defined them, are immutable.  To "add" a new test case
result to a test suite, one must create a copy of the structure and update the
fields as needed.  The `update-tsr` function does this: it takes an existing
`tsr` and updates the duration and various counters based on a new test case
result and duration.

```racket
(define (update-tsr ts-result tc-result tc-duration)
  (match-define (tsr name timestamp duration total failures errors tcrs) ts-result)
  (struct-copy
   tsr ts-result
   ;; Add the current duration to the current duration
   [duration (+ duration tc-duration)]
   ;; Increment the total number of test cases
   [total (add1 total)]
   ;; If this was a failure, increment the number of failures
   [failures (if (test-failure? tc-result) (add1 failures) failures)]
   ;; If this was an error, increment the number of errors
   [errors (if (test-error? tc-result) (add1 errors) errors)]
   ;; Add the test case result to the list of results for this test
   ;; suite
   [tcrs (cons (tcr tc-result tc-duration) tcrs)]))
```

Finally, the `test-result-collector%` needs to be updated to collect the
information about the test results: (1) A current test suite is maintained in
`current`, (2) since `on-test-case-completed` does not receive a test suite
name, suites that have been completed are stored in `completed`, and (3) the
`aside` list contains test suites which are set aside, because an inner test
suite is being run.  The various methods of `test-result-collector%` simply
update these three structures as needed:

```racket
(define test-result-collector%
  (class object% (init [only #f] [exclude exclude]) (super-new)
    (define current #f)                 ; the current TSR
    (define completed '())              ; completed TSRs
    (define aside '()) ; TSRs that we set aside while running nested test suites
    (define/public (get-test-suite-results) completed)
    (define/public (on-test-suite-start name)
      (printf "*** Testsuite ~a~%" name)
      (when current (set! aside (cons current aside)))
      (set! current (fresh-tsr name)))
    (define/public (on-test-suite-completed name)
      (match-define (tsr name timestamp duration total failures errors skipped tsrs) current)
      (printf "*** Testsuite ~a completed in ~a ms~%*** Total tests: ~a, failures: ~a, errors: ~a, skipped: ~a~%"
              name (~r duration #:precision 2) total failures errors skipped)
      (set! completed (cons current completed))
      (if (null? aside)
          (set! current #f)
          (begin
            (set! current (car aside))
            (set! aside (cdr aside)))))
    (define/public (before-test-case-start name)
      (printf "\t~a: " name)
      #t)
    (define/public (on-test-case-completed result duration)
      (printf "~a (~a ms)~%" (test-result-tag result) (~r duration #:precision 2))
      (test-log! (member (test-result-tag result) '(ok skipped)))
      (maybe-display-result result)
      (set! current (update-tsr (or current (fresh-tsr "unnamed test suite")) result duration)))
    ))
```

Collecting the test results has no visible effect on the program output, but
it opens the possibility of publishing the test results in a machine-readable
format which is understood by other systems.

### Interlude: Reporting Test Results to Azure Devops

A build pipeline can report test results to Azure DevOps by having the tests
write their results in one of their supported formats and uploading it using a
`PublishTestResults` task.  Here is an example of publishing the test results
for running the "aggregate-test" in [ActivityLog2][al2]:

```yaml
  - task: PublishTestResults@2
    condition: succeededOrFailed()
    inputs:
      testResultsFiles: 'test/test-results-aggregate.xml'
      testRunTitle: 'aggregate-test on $(Agent.OS)'
    displayName: Publish Test Results (aggregate-test)
```

The task expects the test results to be present in the
"test-results-aggregate.xml" file and one of the formats supported is the
[JUnit result format][junit-schema].  The next step is to update our test
runner to write the results to file.

### Writing an XML Document

The simplest way to write an XML document in Racket is to produce an XEXPR,
which is just a nested Racket list, than use `write-xml`[racket] to write the
document.  The invocation is not entirely trivial, but not very complex
either:

```racket
(define (save-xexpr-to-file xexpr file-name)
  (call-with-output-file file-name
    (lambda (out)
      (write-xml
       (document
        (prolog
         (list (p-i #f #f 'xml "version=\"1.0\" encoding=\"utf-8\""))
         #f '())
        (xexpr->xml xexpr)
        '())
       out))
    #:exists 'replace))
```

All we need now is to produce XEXPR structures for our test case and test
suite results.  Let's start with the test case result: this function outputs a
`<testcase>` XML tag, containing information about a test case.  If there is
an error or a failure, the detailed information is also stored in the XML tag:

```racket
(define (tcr->junit-xexpr data)
  (match-define (tcr result duration) data)
  `(testcase
    ((name ,(test-result-test-case-name result))
     (time ,(~a (/ duration 1000.0))))     ;; time must be in seconds
    ,(cond ((test-failure? result)
            `(failure
              ((message ,(test-result-message result)) (type "failure"))
              ,(error->string result)))
           ((test-error? result)
            `(error
              ((message ,(test-result-message result)) (type "error"))
              ,(error->string result)))
           (#t ""))))
           
(define (error->string result)
  (call-with-output-string
   (lambda (out)
     (parameterize ([current-error-port out])
       (maybe-display-result result)))))
```

A `<testsuite>` XML tag contains information about all the test cases in a
test suite along with the number of tests and errors plus some other
bookkeeping information, such as the time when the test started and the host
on which the test suite run.  The JUnit XML Schema requires a package name to
be present in the test results.  The rackunit test suites don't have this
information, so the package name is supplied to `tsr->junit-xexpr` as a
function argument:

```racket
(define (tsr->junit-xexpr result package id)
  (match-define (tsr name timestamp duration total failures errors skipped results) result)
  `(testsuite
    ((name ,name) (package ,package) (id ,(~a id))
     (timestamp ,(->timestamp timestamp)) (hostname ,(gethostname))
     (tests ,(~a total)) (failures ,(~a failures)) (errors ,(~a errors))
     (time ,(~a (/ duration 1000.0)))) ; time must be in seconds
    ,@(map tcr->junit-xexpr (reverse results))))
    
;; Convert a UTC timestamp (in milliseconds) into a string suitable for
;; writing to the XML file.
(define (->timestamp utc-milliseconds)
  (define (fmt num width)
    (~a #:width width #:align 'right #:pad-string "0" num))
  (let ((d (seconds->date (exact-truncate (/ utc-milliseconds 1000.0)) #f)))
    (string-append
     (fmt (date-year d) 4) "-" (fmt (date-month d) 2) "-" (fmt (date-day d) 2)
     "T"
     (fmt (date-hour d) 2) ":" (fmt (date-minute d) 2) ":" (fmt (date-second d) 2) "Z")))
```

Finally, all the test suites are wrapped in a `<testsuites>` XML tag.  The
package name is also supplied as an argument, as it is required by the JUnit
schema, since the rackunit library has no concept of a package:

```racket
(define (tsrs->junit-xepr tsrs package)
  `(testsuites
    ()
    ,@(for/list ([(tsr id) (in-indexed (reverse tsrs))])
        (tsr->junit-xexpr tsr package id))))
```

Finally, the `run-tests` function can be updated to allow specifying an output
file where the results are written, as well as the package name.  Since most
build systems can collect build results from files on disk, this is sufficient
to integrate the test runner into build pipelines, although other
implementations are possible where results are directly uploaded to some
online service:

```racket
(define (run-tests #:package [package "unnamed package"]
                   #:results-file [results-file #f]
                   . test-suites)
  (parameterize ([current-output-port (current-error-port)])
    (define collector (new test-result-collector%))
    (for ([ts (in-list test-suites)])
      (fold-test-results ts collector))
    (when results-file
      (printf "*** Writing results to ~a~%" results-file)
      (define xexpr (tsrs->junit-xepr (send collector get-test-suite-results) package))
      (save-xexpr-to-file xexpr results-file))))
```

### One Last Thing: Skipping Tests

In a complex application, there are many reasons to temporarily disable tests,
but the rackunit way to deal with this is to just comment out the test.  This
is not ideal, since it is easy to forget about a commented out test, as it is
not failing and not reported anywhere as being disabled.

The JUnit schema and Azure DevOps allow marking a test as "skipped", meaning
that the test is present, but it did not run.  A skipped test will show up in
reports as "skipped" and it is less likely to be ignored long term.  Rackunit
has no direct support for skipped tests, but they can be implemented in the
custom test runner.  This implementation is not very complex, but this blog
post has already become too long, so it will just be outlined here.  For the
full details, you can have a look at the [custom-test-runner.rkt][ctr] which
implements all the ideas presented here.

There are two possibilities to skip a test (1) mark the test as "skipped" and
(2) allow the test to report a "skipped" error code.  The first case is
obvious: perhaps the test is failing and we are not ready to fix it yet.  The
need for the second case is less obvious, but a use case is presented by
[ActivityLog2][al2], where some tests run against database containing private
data and the build pipeline is set up such that this data is not downloaded
for builds outside the main repository.  These tests cannot run successfully
on builds outside the main repository.  These are not failed tests and they
are run by the test runner, yet they cannot test anything.  These test will
report as "skipped" when they don't have the necessary data to run.

The first case is easy to implement: `run-tests` accepts a list of tests to
omit using the `#:exclude` argument, the `before-test-case-start` method is
updated to consult this list and return `#f` for tests that are not supposed
to be run and also update the results to record that the test is skipped.  The
`run-tests` function also has a mechanism to specify only a subset of tests to
run, using the `#:only` argument -- this feature is useful when debugging a
particular test in a test suite that has a long running time (and ActivityLog2
has lots of those.)

The second case, allowing a test to report itself that it should be skipped,
is implemented by raising a special exception, `exn:test:skip` -- rackunit
will consider that the test error-ed, but our test runner can inspect the
exception and conclude that the test is skipped.  To implement this, a
`skip-test` function is provided for tests to report that they are skipped,
and the `on-test-case-completed` method is updated to record this information.

## Final thoughs

The [custom-test-runner.rkt][ctr] provides the necessary glue between writing
[rackunit][rackunit] tests using a familiar library and collecting and systems
that can be used to manage tests and test runs when programs grow larger,
providing better visibility into which tests are run, which ones are skipped
and how the test performance has evolved over time.  At this stage, the runner
may not be sufficiently general for all complex use cases, but I believe that
it can be easily extended to meet future needs.

[rackunit]: https://docs.racket-lang.org/rackunit/api.html
[ru-checks]: https://docs.racket-lang.org/rackunit/api.html#%28part._.Checks%29
[test-case]: https://docs.racket-lang.org/rackunit/api.html?#(form._((lib._rackunit%2Fmain..rkt)._test-case))
[test-suite]: https://docs.racket-lang.org/rackunit/api.html?#(form._((lib._rackunit%2Fmain..rkt)._test-suite))
[junit-schema]: https://github.com/windyroad/JUnit-Schema/blob/master/JUnit.xsd
[al2]: https://github.com/alex-hhh/ActivityLog2
[foldts]: https://docs.racket-lang.org/rackunit/internals.html#%28def._%28%28lib._rackunit%2Fmain..rkt%29._foldts-test-suite%29%29
[ctr]: https://github.com/alex-hhh/ActivityLog2/blob/master/test/custom-test-runner.rkt
[az-common]: https://github.com/alex-hhh/ActivityLog2/blob/master/etc/scripts/azure-common.yml

