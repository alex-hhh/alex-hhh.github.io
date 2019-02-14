    Title: An Overview of Common Racket Data Structures
    Date: 2019-02-14T16:42:38
    Tags: racket
    Thumbnail: /img/a021/thumb.png

The Racket language provides a variety of ready-to-use data structures and
containers, however each such container has advantages and disadvantages when
it comes to performance.  The "everything is a list" approach makes things
simple when you are learning Racket, but if you start using it for more
complex programs, you need to be aware that lists are not always the best data
structure to use.  This guide looks at a few alternatives.

<!-- more -->

There are many data structures available in Racket, but this guide looks at
only three of them: lists, vectors and hash-tables.  These are readily
available with every Racket installation, are versatile and, used carefully,
have good performance characteristics.

## Lists

Lists are perhaps the most popular data structure used in Racket programs plus
Lisp and Scheme too.  If you learned Scheme from classics such as [HTDP][htdp]
or [SICP][sicp], you have seen them used a lot.  The Racket documentation
provides an [overview][lo], there is also an [overview on cons and pairs][co],
plus [reference][lr] documentation for them.  Lists are a very flexible data
structure, and Racket has lots of functions that manipulate them: you can
construct, search and reference elements in a variety of ways and for this
reason you will find lists used in most example programs.

Not all operations on lists are efficient, and using lists containing a lot of
elements can quickly result in performance issues.  If the size of the dataset
is more than a few hundred items, you need to be aware of the following
performance considerations.

**Constructing lists** is only efficient when it is done by adding an element
to the front of the list, using `cons`[racket].  If you need to construct a
list by adding an element to the end of the list, it will be more efficient to
construct the list in reverse first, using `cons`[racket], than reverse the
list at the end.  The example below constructs a list of 10 integers by
accumulating the result in reverse, than reverting the result at the end to
produce the final list.

```racket
(define my-list
  (let loop ([accumulator '()]
             [index 0])
    (if (< index 10)
        (loop (cons index accumulator) (add1 index))
        (reverse accumulator))))
```

The above pattern might be difficult to read and understand and use for
complex scenarios, so consider using `for/list`[racket] or `for*/list`[racket]
to construct lists by iteration.

**Iterating over all the elements in a list**, using `map`[racket] or
`for-each`[racket] is usually efficient, however if you need to traverse a lot
of large collections, a `vector` might be a better alternative.  When using
[iterations and comprehensions][for], like `for`[racket] or `for/list`[racket]
to iterate over lists, consider using an `in-list`[racket] sequence, as this
is significantly more efficient, than just passing the list itself to the
`for`[racket] construct.  For example, use:

```racket
(for ([element (in-list my-list)))
  (prinf "~a~%" element))
```

**Searching for an element in a list** using `member`[racket], `findf`[racket]
and related functions takes a linear time and this can become a problem for
large lists.  If you need to do a lot of lookups, consider using a
`hash`[racket] or `hash-set`[racket] instead.

**Referencing an element by its position**, using `list-ref`[racket] takes a
linear time and will become slower as the referenced element is further down
the list.  If you need to reference elements by their position, consider using
a `vector`[racket].

**Inserting, updating, deleting elements and appending lists** are inefficient
for large lists.  In Racket, lists are immutable, which means that whenever
you insert, delete an element from a list, an entire new list is constructed
and returned and for large lists, this is very inefficient.  Appending a list
to another also has to create a new list, making it an inefficient operation.
If you need to insert, update or delete elements in a container, consider
using a `hash`[racket] or a `hash-set`[racket] instead.  A `vector`[racket] is
also efficient at updating elements.
  
## Vectors

[vectors][vo] are fixed length arrays of arbitrary values and element access
and update is done in a constant time.  They are a versatile and efficient; in
languages such a Python and C++, vectors are probably the most used container.
Note that what Python calls "lists" are really implemented as vectors -- this
can be confusing if you just start out with Racket, as Python "lists" have
different performance characteristics (they are really vectors).

The [reference][vr] documentation lists all the functions available on vectors
and these are similar to the functions that operate on lists (for example the
equivalent of `map`[racket] function for vectors is `vector-map`[racket]).  In
Racket, vectors can be either **mutable** or **immutable** -- immutable
vectors cannot have their elements changed after they are created.

Just as with lists, some operations are provided for convenience, but they are
not very efficient and you need to be aware of these.

**Constructing vectors** requires the size to be known in advance, and this
size cannot be changed after a vector is constructed, although, Racket also
provides a `gvector`[racket] which can grow.  To construct a vector, you can
use one of the following:

* `make-vector`[racket] to construct a vector where all elements have the same
  value

* `build-vector`[racket] to construct a vector with elements provided by a
  user supplied function
  
* the `for/vector`[racket] form will construct by iterating over some
  sequences.  In general, this form is convenient if you need to construct a
  vector without knowing the final number of elements, but it will be
  inefficient.  However, the form has a `#:length` keyword which can be used
  to provide the length, making the construction efficient.

**Iterating over all the elements in a vector** can be done using [iterations
and comprehensions][for], like `for`[racket], however, when doing so, consider
using an `in-vector`[racket] sequence, as this is significantly more
efficient, than just passing the vector itself to the `for`[racket] construct.
For example, use:

```racket
(define my-vector (build-vector 10 (lambda (i) i)))

(for ([element (in-vector my-vector)))
  (prinf "~a~%" element))
```

Note that the `vector-map`[racket] function will produce a new vector with the
results of the map function, so it is not a good idea to use it just for
simply iterating over the elements.  `vector-map!`[racket] can be used
however, to update all elements in-place.

**Searching for an element in a vector** can be done using
`vector-member`[racket] or `vector-memq`[racket] but, just like with lists,
these functions need to scan the vector linearly and will be inefficient for
large vectors.  Still, a vector will be more efficient at this linear search
than a list with a similar number of elements, as elements in a vector (or at
least their references) are next to each other and will have better cache
access characteristics in the computer memory.

**Referencing an element by its position**, using `vector-ref`, is an
efficient operation, and if you need to do that often, vectors are the best
container for this.

**Updating an element** in a vector using `vector-set!` is efficient, but
**inserting or deleting elements or appending two vectors** are inefficient as
they have to construct new vectors, you should avoid them in performance
sensitive code, especially with vectors containing a large number of elements.

**Other vector types, such as `gvector` and `flvector`** have better
performance characteristics for some use cases.  Two of the most important of
these are `gvector` and `flvector`.

A [gvector][gvr] is a vector data type that supports efficient appending of
elements at the end of the vector.  It is great for constructing vectors when
you don't know the final number of elements.

If you need to store only floating point values in your vector, consider using
a [flvector][flvr] which provides a more compact and efficient vector
representation for these values.  A corresponding suite of functions is
available for operating on `flvector` instances, see the reference for more
details.

If the program you write has a lot of numerical calculations, consider using
the [flonum][fl] library, which provides operations for floating point numbers
(for example, it provides a `fl+`[racket] operation which is more efficient
than the `+` operation).

## Hash Tables

Hash tables are containers which map keys to values and can lookup keys
efficiently.  The Racket manuals have [overview][hto] and [reference][htr]
sections for hash tables explaining how to use them and what functions are
available.  Like vectors, hash tables can be mutable or immutable, where
immutable ones cannot be changed after they are created.

Just as with lists and vectors, some operations are provided for convenience,
but they are not very efficient and you need to be aware of these.

**Constructing hash tables** can be efficiently done by creating an empty hash
table first and adding elements to it one by one.  Note that the
`for/hash`[racket], while convenient to use will actually create an immutable
hash table, so you cannot add any elements to it after it was created.

**Iterating over all the elements in a hash table*** can be done using
[iterations and comprehensions][for], but consider using `in-hash`[racket],
`in-hash-keys`[racket], or `in-hash-values`[racket] for these `for`[racket]
loops.  Avoid using `hash-keys`[racket] or `hash-values`[racket] as these
functions construct an intermediate list, which can be a performance problem.
For example, to print out all elements in a hash table, you can use:

```racket
(for [(k v) (in-hash h)]
  (printf "key: ~a, value ~a~%" k v))
```

**Searching for an element in a hash table** using `hash-ref`[racket] is
efficient.  If you need to scan all elements to look for a value, this will
take a linear amount of time, it will be inefficient and should be avoided.
If you do need to scan all elements to look for one, at least consider using a
`for`[racket] form and `in-hash-values`[racket] -- the alternative of
obtaining a list of elements and scanning them using list operations has even
worse performance characteristics.

**Inserting, updating, deleting elements** can be done efficiently, so if you
need to perform a lot of these operations, a hash table is the best container
to use.  Here is a summary of the available operations:

* `hash-ref`[racket] will find an element by a key.  By default, if the
  element is not found, an error is raised, but you can specify what value to
  return if the key is not present.  For example to return `#f` when a key is
  not found, use `(hash-ref a-hash key #f)`
  
* `hash-ref!`[racket] is similar to `hash-ref`[racket] but it will create a
  new value for the key, of one does not already exist.  It will be more
  efficient to use this instead of checking if a key exists and inserting it
  if it does now.
  
* `hash-set!`[racket] will set a value for a key, possibly replacing an old
  value.

* `hash-update!`[racket] combines can be used to update or create a new value,
  it effectively combines `hash-ref`[racket] and `hash-set!`[racket] into one
  operation.
  
* `hash-remove!`[racket] will remove an element from the hash table

* `hash-clear!`[racket] will remove all elements in a hash table and will be
  more efficient than removing them one by one using `hash-remove!`.

**Appending hash tables** can be done using `hash-union`[racket] or
`hash-union!`[racket], but these will not be very efficient for large hash
tables, however, `hash-union!` can be used for efficiently appending a smaller
hash table to a larger one.

## Conclusion

One can write performance sensitive applications in Racket, but the choice of
data structures and algorithms will greatly affect the final performance.
This post is a high level overview of the most important data structures and
provided some guidelines on how to choose between them, but ultimately, the
final choice will depend on the type of processing program needs to do and
what algorithms you choose.



[lo]: http://docs.racket-lang.org/guide/Lists__Iteration__and_Recursion.html
[co]: http://docs.racket-lang.org/guide/Pairs__Lists__and_Racket_Syntax.html
[lr]: http://docs.racket-lang.org/reference/pairs.htm
[flvr]: http://docs.racket-lang.org/reference/flonums.html?#(part._flvectors)
[flr]: http://docs.racket-lang.org/reference/flonums.html
[vr]: http://docs.racket-lang.org/reference/vectors.html
[vo]: http://docs.racket-lang.org/guide/vectors.html
[gvr]: http://docs.racket-lang.org/data/gvector.html
[hto]: http://docs.racket-lang.org/guide/hash-tables.html
[htr]: http://docs.racket-lang.org/reference/hashtables.html
[htdp]: https://www.htdp.org/
[sicp]: https://mitpress.mit.edu/sites/default/files/sicp/index.html
[for]: https://docs.racket-lang.org/reference/for.html
