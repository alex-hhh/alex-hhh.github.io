    Title: More Timezone Lookup (loading and saving data)
    Date: 2019-08-10T08:17:06
    Thumbnail: /img/a028/thumb.png
    Tags: racket

... in which we explore how to write internal Racket data structures to disk
and read them back, and use these features to improve load times for the
GeoJSON data, so the program does not have to spend 20 seconds in the
initialization step.

<!-- more -->

In the [previous blog][tz] post we looked at how to perform a geo-lookup and
determine the time zone corresponding to some GPS coordinates.  Various
improvements were done over the initial search algorithm, and, in the end, the
average time to perform the timezone lookup was reduced substantially.

So far, two aspects of the lookup function have been ignored: it takes a long
time to load and prepare the lookup data, about 20 seconds, and the size of
the GeoJSON file containing the timezone boundaries is about 129Mb which is
very large.  This large initialization time means that there is still a large
cost for using the lookup function in an application which needs to do just a
few lookups and the large GeoJSON file has two further implications: (1) the
size of the package on disk is too big and (2) memory usage of the application
is also large, since all that data has to be loaded in the program memory.

In this blog post we'll look at how to improve the load times: we'll try to
use the `read`[racket] and `write`[racket] built-in Racket functions to save
the prepared features to disk once and read them when using the lookup
program, but it turns out that these functions are slower than just reading
the GeoJSON data directly.  Next, we'll try the Fast-Load Serialization
functions and these will provide significantly faster load times.  After that,
we'll explore how to avoid loading the entire data set into memory, and we'll
do that by saving the data for each timezone in a separate file and only load
them when needed.  Finally we'll try to reduce the space used on disk by
compressing the data files and decompressing them in memory.

![World TimeZones](/img/a028/timezone-map-2.png)

## Short refresher

The setup process for the timezone lookup was split in two functions:
`load-geojson` loads the GeoJSON timezone data, and just calls
`load-json`[racket], while the `prepare-features` function constructs bounding
boxes and converts polygons from the GeoJSON lists to vectors, for faster
lookups.  See the [previous blog post][tz] for details of these functions.
Their running times are shown below:

| **function**     | **total calls** | **total time** *(ms)* |
|------------------|----------------:|----------------------:|
| load-geojson     |               1 |               9791.16 |
| prepare-features |               1 |                8290.5 |

Since the time zone data does not change very often, we could just save the
result of `prepare-features` and load that at startup, saving the time to
prepare the feature data structures each run.  We can split the timezone
lookup process in two: a `tzpack` function will load the GeoJSON timezone
data, prepare the data such that it is convenient to use by the `tzlookup`,
than save it to disk.  The `tzlookup` package would load this data from disk
and use it for lookups.  We would only need to run `tzpack` when a new GeoJSON
dataset is released, so this would save time.

## `read` and `write`

How to save the data?  Well, Racket already provides the `write`[racket]
function to write an internal data structure to an output stream and the
corresponding `read`[racket] function to read it back.  There are some
limitations on what data can be serialized this way, but this includes
structures, vectors and lists, which is what we use, so these functions are
adequate for our purpose.  The only change is that we have to tag our
structures with the `#:prefab` keyword, but this does not affect the rest of
the code:

```racket
(struct bbox (min-x min-y max-x max-y) #:prefab)
(struct polygon (bbox points) #:prefab)
(struct shape (outline holes) #:prefab)
(struct feature (name shapes) #:prefab)
```

We can than load the GeoJSON, prepare the features and save it all to disk, in
the "tzdata.dat" file, in only a few lines of code:

```racket
(define features
  (let ([tzdata (load-geojson "./data/combined.json" #:verbose #t)])
    (prepare-features tzdata)))

(call-with-output-file "./tzdata.dat"
  (lambda (out) (write features out))
  #:exists 'replace)
```

Running the above program will require the `load-geojson`, `prepare-features`
and related functions presented in the [previous blog post][tz], and will
produce a data file which is 117Mb in size, which is smaller than the original
129Mb GeoJSON file, but not by much, in both cases we could compress the
resulting files, reducing their size on disk, but they would have to be stored
uncompressed in memory anyway, so these numbers are also indicative of the
approximate memory usage of the program.

The lookup program will have to load this data, which in this case it involves
calling `read`[racket] to load the data into the `features` global variable.
The rest of the program will remain unchanged:

```racket
(define (load-data path)
  (call-with-input-file path read))

(define features (load-data "./tzdata.dat"))
```

How does this compare?  Well, the running times are below and a few things are
expected: `load-geojson` and `prepare-features` are no longer called, since
they were replaced by `load-data` and the rest of the running times are about
the same, since the `tz-lookup` operates on the same data structures.
Unfortunately, loading the data takes 38 seconds, which is much longer than
loading the GeoJSON and preparing the features each time, so using
`write`[racket] and `read`[racket] to write a complex data structure to disk
and read it back is easy, but the result is not very fast:


| **function**                    | **total calls** | **total time** *(ms)* | **min** *(ms)* | **max** *(ms)* | **average** *(ms)* |
|---------------------------------|----------------:|----------------------:|---------------:|---------------:|-------------------:|
| load-data                       |               1 |              37947.24 |       37947.24 |       37947.24 |           37947.24 |
| tz-lookup                       |            1038 |               30403.9 |           1.06 |         247.86 |              29.29 |
| feature-winding-number          |          442188 |              30208.83 |              0 |         141.35 |               0.07 |
| shape-winding-number            |         1223802 |              29746.79 |              0 |         141.34 |               0.02 |
| polygon-winding-number          |         1497797 |              29169.28 |              0 |         141.34 |               0.02 |
| polygon-winding-number-internal |            1706 |               28649.1 |              0 |         141.33 |              16.79 |
| subtended-angle                 |        55104870 |              10463.29 |              0 |           2.88 |            0.00019 |


## Fast-Load Serialization

The output produced by `write`[racket] is indented to be somewhat human
readable and this might contribute to it being slow.  Racket also provides
some serialization/deserialization routines which produce binary data and
promise to be faster.  They are part of the `racket/fasl` library and using
them is as simple as replacing `write`[racket] with `s-exp->fasl`[racket] and
`read`[racket] with `fasl->s-exp`[racket].  The `tzpack` program becomes:

```racket
(require racket/fasl)

(define features
  (let ([tzdata (load-geojson "./data/combined.json" #:verbose #t)])
    (prepare-features tzdata)))

(call-with-output-file "./tzdata.dat"
  (lambda (out) (s-exp->fasl features out))
  #:exists 'replace)
```

And loading the data in the lookup program becomes:

```racket
(define (load-data path)
  (call-with-input-file path fasl->s-exp))

(define features (load-data "./tzdata.dat"))
```

----

Unfortunately, fast-load serialization does not support floating point
vectors, but since using flvectors turned out to be a performance loss, I
reverted the code to use plain vectors.

----

Using `s-exp->fasl`[racket] results in a 104Mb data file, which is smaller
than the one created by `write`[racket], which was 117Mb.  Loading the data is
also faster, at 1.2 seconds, which is about 30 times faster than
`read`[racket] and about 15 times faster than the loading the GeoJSON itself
and preparing the data set:

| **function**                    | **total calls** | **total time** *(ms)* | **min** *(ms)* | **max** *(ms)* | **average** *(ms)* |
|---------------------------------|----------------:|----------------------:|---------------:|---------------:|-------------------:|
| load-data                       |               1 |               1217.51 |        1217.51 |        1217.51 |            1217.51 |
| tz-lookup                       |            1038 |              26628.92 |           1.01 |         164.12 |              25.65 |
| feature-winding-number          |          442188 |              26459.86 |              0 |          89.83 |               0.06 |
| shape-winding-number            |         1223802 |               26048.1 |              0 |          89.82 |               0.02 |
| polygon-winding-number          |         1497797 |              25529.97 |              0 |          89.81 |               0.02 |
| polygon-winding-number-internal |            1706 |              25326.43 |              0 |          89.81 |              14.85 |
| subtended-angle                 |        55104870 |               9462.48 |              0 |           3.82 |            0.00017 |

## Runtime memory use

Each time we use the timezone lookup function, we must load the entire data
set, which is about 100Mb on disk.  How much memory does it use?  Well, the
entire data set has 5'728'474 GPS points, and since each latitude/longitude
coordinate is represented as a pair of double-precision floating point number,
which are 8 bytes each, just storing the points will take up 87 Mb of memory,
with the actual memory use being higher since there is some overhead for the
additional data structures such as bounding boxes.  It looks like the size of
the serialized data is a pretty good indication of the memory used for time
zone lookup.

While the 1.2 seconds it takes to read the data might be sufficient, the high
memory use will probably not be acceptable for an application which needs to
do a lookup occasionally.

How can we reduce memory use?  The basic idea is to only load the data we
actually need: when we introduced bounding boxes, it meant that entire
polygons were skipped by `polygon-winding-number` and perhaps we can avoid
loading these polygons altogether.

Unfortunately, Racket does not provide direct facilities for only reading data
that we need, both `fasl->s-exp`[racket] and `read`[racket] will read the
entire data set.  We can address this limitation by clever use of these
functions: The `features` variable stores a list of `feature` structures which
are inspected one-by-one by `tz-lookup`, but we can change that so that (1)
each `feature` structure is saved in a separate file and (2) having an index
with the bounding boxes and the features to check.

Saving individual features, means that we'll need to call
`s-exp->fasl`[racket] on each individual feature and save it into a separate
file.  File names are constructed from the actual time zone names by replacing
the "/" (which is invalid in a file name) with a "+" sign:

```racket
(define (save-data data file-name)
  (call-with-output-file file-name
    (lambda (out) (s-exp->fasl feature out))
    #:exists 'replace))

(define features
  (let ([tzdata (load-geojson "./data/combined.json" #:verbose #t)])
    (prepare-features tzdata)))
    
(make-directory* "./pack")

(for ([feature (in-list features)])
  (define file-name (format "pack/~a.dat" (string-replace (feature-name feature) "/" "+")))
  (save-data feature file-name))
```

To build the index, we'll need to scan all the features and find the polygon
bounding box for each shape outline, and produce a list with the feature name
and the bounding box itself.  The index itself is saved into a separate file,
"index.dat", also using `s-exp->fasl`[racket]:

```racket
(define index '())
(for ([feature (in-list features)])
  (define name (feature-name feature))
  (for ([shape (in-list (feature-shapes feature))])
    (define outline (shape-outline shape))
    (set! index (cons (cons name (polygon-bbox outline)) index))))

(save-data "pack/index.dat")
```

While the total data size is still the same, each timezone definition is in
its own separate file, which means that it can be loaded individuality, while
the index file is only 56Kb in size.

The lookup functionality will have to change, since we changed the the way we
store the data: we will load the index first, and just prepare a hash table
for the features that will be loaded.  Initially, the hash will be empty:

```racket
(define index (load-data "./pack/index.dat"))
(define features (make-hash))
```

The `tz-lookup` function will have to scan the index first, and only check
features for which the location to be tested is inside the bounding box:

```racket
(define (tz-lookup lat lon)
  (define candidates
    (for/fold ([result '()])
              ([entry (in-list index)])
      (match-define (cons name bbox) entry)
      (if (inside-bbox? bbox lat lon)
          (let ((wn (feature-winding-number name lat lon)))
            (cons (list name wn) result))
          result)))

  ;; filtering and selecting candidates remains the same
  )
```

And, finally, the `feature-winding-number` needs to be updated to accept a
time zone name (instead of the feature structure itself), lookup the feature
and load it if needed.  `feature-winding-number-internal` contains the old
body, which evaluates the actual feature struct:

```racket
(define (feature-winding-number feature-name lat lon)
  (define feature (hash-ref features feature-name #f))
  (unless feature
    (define file-name (format "pack/~a.dat" (string-replace feature-name "/" "+")))
    (set! feature (load-data file-name))
    (hash-set! features feature-name feature))
  (feature-winding-number-internal feature (exact->inexact lat) (exact->inexact lon)))
```

And below are the performance results for the updated code. `load-data` is now
called multiple times, to load both the index and the features, as they are
needed, however, on average this call only takes 2.35 milliseconds now, since
it has to load much less data each time.  The time for the `tz-lookup` call
went up by 5 milliseconds, since the function may have to load some timezone
data if it is not already in the cache, but the average time is still under 30
milliseconds, which is pretty good, with even the maximum time of 234
milliseconds probably being acceptable for many applications -- this time
corresponds to a call that had to load one or more data files.


| **function**                    | **total calls** | **total time** *(ms)* | **min** *(ms)* | **max** *(ms)* | **average** *(ms)* |
|---------------------------------|----------------:|----------------------:|---------------:|---------------:|-------------------:|
| load-data                       |             234 |                549.56 |           0.07 |          25.34 |               2.35 |
| tz-lookup                       |            1038 |              31114.38 |           0.04 |         234.84 |              29.98 |
| feature-winding-number          |            1706 |              31033.07 |              0 |         230.89 |              18.19 |
| feature-winding-number-internal |            1706 |              30466.94 |              0 |         230.89 |              17.86 |
| shape-winding-number            |            6627 |              30460.42 |              0 |         230.89 |                4.6 |
| polygon-winding-number          |            9534 |              30452.65 |              0 |         230.88 |               3.19 |
| polygon-winding-number-internal |            1724 |              30446.66 |              0 |         230.88 |              17.66 |
| subtended-angle                 |        55328096 |              11153.11 |              0 |         224.75 |            0.00020 |


## Compressing the data files

The data files produced by `s-exp->fasl`[racket] are about 102Mb in size, and
while this is smaller than the original 129Mb for the GeoJSON file, they are
still a sizable chunk.  A quick test, indicated that compressing these files
using `gzip` would reduce their size to about 62Mb -- this is still big, but
it will save about 40Mb, which is worth doing since Racket has built-in file
compression routines, and we can use them to compress the data as it is
written out and decompress it as it is read in.

The compression is done using `gzip-through-ports`[racket], which requires an
input port and output port; the output port will be the file itself, and is
provided by `call-with-output-file`, while the input port will be provided
from a byte buffer using `call-with-input-bytes`.  The byte buffer itself will
be created by calling `s-exp->fasl`[racket] with an byte output buffer, via
`call-with-output-bytes`.

```racket
(require file/gzip)

(define (save-data data file-name)
  (define buffer (call-with-output-bytes (lambda (out) (s-exp->fasl data out))))
  (call-with-output-file file-name
    (lambda (out)
      (call-with-input-bytes
       buffer
       (lambda (in)
         (gzip-through-ports in out file-name (current-milliseconds)))))
    #:exists 'replace))
```

To read the data, we'll need to use `gunzip-through-ports`[racket] and perform
all the steps in reverse:

```racket
(require file/gunzip)

(define (load-data path)
  (define data
    (call-with-output-bytes
     (lambda (out)
       (call-with-input-file path
         (lambda (in)
           (gunzip-through-ports in out))))))
  (call-with-input-bytes data fasl->s-exp))
```

These changes are isolated to the `save-data` and `load-data` functions only,
nothing else needs to change, so it is a good disk space saving with a minimum
of effort.  How does the performance change?  The full performance results re
below, and we can see that the average time for `load-data` has gone up to 18
milliseconds from 2 milliseconds, since it now has to decompress the data.
The time for `tz-lookup` has also gone up by 5 milliseconds on average, since
it has to call a slower `load-data` for some of the lookups, but not for all
of them.  The price we paid for smaller disk usage was a higher lookup time.


| **function**                    | **total calls** | **total time** *(ms)* | **min** *(ms)* | **max** *(ms)* | **average** *(ms)* |
|---------------------------------|----------------:|----------------------:|---------------:|---------------:|-------------------:|
| load-data                       |             234 |               4304.51 |           0.13 |         270.68 |               18.4 |
| tz-lookup                       |            1038 |               35882.5 |           0.05 |         375.55 |              34.57 |
| feature-winding-number          |            1706 |              35794.07 |           0.01 |         288.66 |              20.98 |
| feature-winding-number-internal |            1706 |              31472.95 |              0 |         127.34 |              18.45 |
| shape-winding-number            |            6627 |               31465.5 |              0 |         127.32 |               4.75 |
| polygon-winding-number          |            9534 |              31457.16 |              0 |         127.32 |                3.3 |
| polygon-winding-number-internal |            1724 |              31450.41 |              0 |         127.32 |              18.24 |
| subtended-angle                 |        55328096 |               11338.9 |              0 |          50.44 |            0.00020 |

## Final Thoughts

Loading the GeoJSON files and preparing the data for use by the `tz-lookup`
function took about 20 seconds, which meant that there was a large upfront
"cost" of using the lookup function.  First, we split the task into two
programs: one which loads the GeoJSON file, packs it and saves the data to
disk, and another program which loads the data and does the lookups.  Since
the GeoJSON data changes only a few times a year, the packed data will only
rarely need to be updated and can be shipped with the package.  Next, we
reduced the startup time to a negligible amount by using Fast-Load
Serialization and by splitting the data into separate files, but as a result
of this, the lookup function is slightly slower since it now has to load the
data as it is needed.  Optimization is often about making trade-offs, but I
believe that the slower lookup times are worth the benefit of a very fast
initialization time, since the lookup is still only 34 milliseconds on
average.

In the end the required disk space to store the data was about 62Mb which is
still somewhat large, but since disks now have hundreds of gigabytes of disk
space, perhaps not such a big issue.  Still, this is an area of improvement.
It is worth mentioning here that there are [timezone lookup
packages][tz-lookup] which use a much smaller amount of disk space, however,
these packages trade off accuracy in non-populated areas and discard
information when they store the data on disk -- this trade-off might be a
valid one when disk space is at a premium, but this blog post focused on
maintaining full accuracy everywhere.

This and the [previous blog post][tz] went through about 10 revisions of the
timezone lookup programs, and the unit test suite that was written at the
start proved itself very useful, as I could confidently make changes to the
program, knowing that the program still performed its intended function, to
resolve GPS coordinates to timezones.

Finally, the sources for the programs presented in this Blog Post are
available [here][tztests].

[tz]: /2019/08/timezone-lookup.html
[tztests]: https://github.com/alex-hhh/time-zone-lookup-tests
[tz-lookup]: https://github.com/darkskyapp/tz-lookup
