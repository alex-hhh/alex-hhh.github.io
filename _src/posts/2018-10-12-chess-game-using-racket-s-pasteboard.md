    Title: Chess Game Using Racket's Pasteboard
    Date: 2018-10-12T19:35:13
    Thumbnail: /img/a017/thumb.png
    Tags: racket

The Racket GUI library provides an "editor toolkit" which can be used to
implement programs that use an interactive graphical canvas where objects can
be moved around with the mouse.  This toolkit has [good reference
documentation](http://docs.racket-lang.org/gui/editor-overview.html), however
this documentation can be somewhat overwhelming, and it is not always clear
how to begin writing such interactive application, or how to achieve some
basic functionality, so I wrote a tutorial on how to implement a chess board
game in Racket using the `pasteboard%` and `snip%` classes which are part of
the GUI library.

<!-- more -->

This tutorial presents a step-by-step guide on how to create a chess board
application using only the Racket GUI toolkit.  While it uses chess as an
example application, it is meant to illustrate how to create new `snip%`
objects and how to manage them inside `pasteboard%` objects, and rather than
enumerating all the features of the `pasteboard%` class, it just shows the
ones that are needed to solve the immediate problems of implementing the
game. The tutorial will not implement a complete chess game, as in you will
not be able to play a game of chess against the computer with this program --
it will only deal with the graphical interface of the chess game.

The topics that I wanted to cover turned out to be too long for a single blog
post, so the final program does not provide a payable game, I plan to write
another blog post covering some more topics related to this.

![](/img/a017/chess-board.png)

## Chess pieces as snip objects

The first thing we need in a chess game are the chess pieces, and to keep the
application simple, we will use the Unicode characters for the chess pieces,
so each piece will actually be displayed as text.  There are 12 chess pieces
in total listed below with their name, mnemonic letter and Unicode code point.
The Unicode glyphs, as displayed below, are too small to be used in the game,
but they can be rendered using a larger font to make them bigger.

| Name         | Mnemonic | Unicode Character Code | Unicode Character |
|:-------------|:--------:|:----------------------:|:-----------------:|
| White King   | K        | #\u2654                | ♔                 |
| White Queen  | Q        | #\u2655                | ♕                 |
| White Rook   | R        | #\u2656                | ♖                 |
| White Bishop | B        | #\u2657                | ♗                 |
| White Knight | N        | #\u2658                | ♘                 |
| White Pawn   | P        | #\u2659                | ♙                 |
| Black King   | k        | #\u265A                | ♚                 |
| Black Queen  | q        | #\u265B                | ♛                 |
| Black Rook   | r        | #\u265C                | ♜                 |
| Black Bishop | b        | #\u265D                | ♝                 |
| Black Pawn   | p        | #\u265F                | ♟                 |



Before we can write the `snip%` for the chess pieces, we need to define a
"snip class".  This name is misleading, as we are not defining a `class%`, but
an instance of the `snip-class%` object.  This "snip class" is used when
serializing snips from the pasteboard.  We won't use this functionality here,
but we still need to define one.  Since we don't use serialization facilities
the definition is very simple, and shown below.  The class name, as passed to
`set-classname` needs to be unique between all the snip classes used by the
application:

```racket
(define chess-piece-snip-class
  (make-object
   (class snip-class%
     (super-new)
     (send this set-classname "chess-piece-snip"))))
```

The snip class object needs to be registered with the Racket editor gui, and
this is done using the call below:

```racket
(send (get-the-snip-class-list) add chess-piece-snip-class)
```

We will use a single snip class for all chess pieces, since the only
difference between them is how they are displayed.  To have a minimal working
snip, three things need to be present in the derived class:

* a call to `set-snipclass` which associates the snip instance with the snip
  class that was previously defined
* a `get-extent` method which the `pasteboard%` uses to determine the size of
  the snip
* a `draw` method, which is used to draw the snip onto the pasteboard canvas.

Our `chess-piece%` snip class receives three arguments in the constructor: a
`glyph`, which is a string representing the Unicode character for the piece, a
`font` used to render the glyph and a `size` which is the size in pixels of
the chess piece (since the piece is a square it will have the same width and
height).  The font object could already determine the size of the piece, but a
separate size parameter allows us to define chess pieces that are larger than
the glyph that they display (as an exercise, you can experiment with different
variations of font and size in the `make-chess-piece` defined later).

The `get-extent` method is used by the `pasteboard%` to obtain the dimensions
of the snip.  The implementation is straightforward, as it just reports the
`size` as both the `width` and `height`, but the actual method call is
somewhat unusual: a device context, `dc` is passed in, together with the
position of the snip on the canvas as the `x` and `y` coordinates, in return,
the `pasteboard%` expects the `width`, `height` and some other parameters to
be filled in by our object (the other parameters have to do with snips that
are part of a text editor and represent text, they don't concern us here, so
they are set to 0).  The method is somewhat unusual as it uses `box`-es for
the output parameters, so we need to use `box-set!` to set the output value.

The `draw` method is used to paint the snip contents onto the canvas.  It
receives the device context, `dc` and the snip position on the canvas.  The
method also receives other parameters which would allow re-drawing only the
parts of the snip that actually need updating, but our code always draws the
entire snip contents.  Our implementation simply draws the glyph in the middle
of the snip area, but nothing else, so the actual snip appears transparent,
with only the chess piece being drawn.

```racket
(define chess-piece%
  (class snip%
    (init-field glyph font size)
    (super-new)
    (send this set-snipclass chess-piece-snip-class)

    (define/override (get-extent dc x y width height descent space lspace rspace)
      (when width (set-box! width size))
      (when height (set-box! height size))
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    (define/override (draw dc x y . other)
      (send dc set-font font)
      (send dc set-text-foreground "black")
      (define-values (glyph-width glyph-height baseline extra-space)
        (send dc get-text-extent glyph font #t))
      (let ((ox (/ (- size glyph-width) 2))
            (oy (/ (- size glyph-height 2))))
        (send dc draw-text glyph (+ x ox) (+ y oy))))
    ))
```

Creating `chess-piece%` instances is inconvenient, as we have to remember the
Unicode glyph for each chess piece, as well as the font and size to use for
them (and preferably to create all pieces of the same font and size).  To
simplify their, we can write a function, `make-chess-piece` which receives the
piece mnemonic (K for king, Q for queen, etc) and creates the piece.  Thus to
create a white king piece, we can just call `(make-chess-piece "K")`:

```racket
(define chess-piece-data
  (hash
   "K" #\u2654 "Q" #\u2655 "R" #\u2656 "B" #\u2657 "N" #\u2658 "P" #\u2659
   "k" #\u265A "q" #\u265B "r" #\u265C "b" #\u265D "n" #\u265E "p" #\u265F))

(define (make-chess-piece id)
  (define glyph (hash-ref chess-piece-data id))
  (define font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
  (new chess-piece% [glyph (string glyph)] [font font] [size 35]))
```

Finally, we can write some test code to check how our snips work: we'll need a
`pasteboard%` instance to hold the chess piece snips, and an `editor-canvas%`
to display the contents of this pasteboard.  As a test, we insert an instance
of each chess piece type, so we can see how they look:

```racket
;; The pasteboard% that will hold and manage the chess pieces
(define board (new pasteboard%))

;; Toplevel window for our application
(define toplevel (new frame% [label "Chess Board"] [width (* 50 8)] [height (* 50 8)]))

;; The canvas which will display the pasteboard contents
(define canvas (new editor-canvas%
                    [parent toplevel]
                    [style '(no-hscroll no-vscroll)]
                    [horizontal-inset 0]
                    [vertical-inset 0]
                    [editor board]))

(send toplevel show #t) ;; show the toplevel frame

;; Insert one of each of the chess pieces onto the board, so we can see them
;; and drag them around.
(for ([id (in-hash-keys chess-piece-data)])
  (define piece (make-chess-piece id))
  (send board insert piece (random (* 50 6)) (random (* 50 6))))
```

You can find the entire program in this [GitHub
Gist](https://gist.github.com/alex-hhh/4b534a8f5633a4ee65a605822a85dfe3) and
if you run it you will get the result as shown below.  With only about 65
lines of code we already have an application which can display chess pieces
and move them around a board using the mouse.  The board however does not look
like a chess board at all, so we need to work on that next.

![](/img/a017/chess-board1low.gif)

## Drawing the chess board

To display the chess board pattern on the background, we can create a derived
`pasteboard%` class and implement the `on-paint` method.  Since the chess
board itself is static and the user does not interact with it, this approach
is the simplest.  The `on-paint` method is invoked twice during each repaint:
once before the snips are painted and once after.  The `before?` parameter
tells us which invocation it is.  The method also receives a device context,
`dc`, plus some other parameters which determine the area that needs
repainting -- these parameters would help in speeding up complex redraw
operations, but since our drawing needs are simple, we just redraw the entire
board every time.  Our implementation simply calls the `draw-chess-board`
function (defined later) when the `on-paint` is invoked to draw the
background:

```racket
(define chess-board%
  (class pasteboard%
    (super-new)
    (define/override (on-paint before? dc . other)
      (when before?
        (draw-chess-board dc)))))
```

The `draw-chess-board` function is shown below.  It paints the checkered
pattern of the board on the entire size of the device context, which is the
canvas area.  To assist the player identifying squares in chess notation, such
as "d3", it also paints the rank and file values on the left and bottom edges.

A few points about this code are worth mentioning:

* The racket device context based drawing follows the same principles as in
  any other language that uses such device contexts: drawing resources such as
  brushes, pens and fonts must be created and set on the device context and
  draw operations such as `draw-rectangle`, or `draw-text` use the last set
  drawing resource.
* Creating drawing resources, such as `pen%`, `brush%` and `font%` objects
  directly is expensive, so Racket provides a "manager" type object which only
  creates them as needed:`the-pen-list`, `the-brush-list`, `the-font-list`.
  Because of this, we don't need to keep these objects around in the class, we
  can simply retrieve them every time `draw-chess-board` is invoked.
* The method clears the device context before drawing -- since the method is
  supposed to cover the entire area of the board and we only draw the "black"
  squares, this is the simplest method to ensure that no previous image data
  is left over from previous drawing operations.
* The code does not assume that the board is a certain size, or even that it
  is a rectangle.  Instead, it determines the size dynamically and calculates
  the explicit width and height of each square, as well as the positions of
  the labels.  This is helpful, this function will not have to change if a
  chess board of a different size is created, and will also handle dynamic
  resizes of the canvas.

```racket
(define (draw-chess-board dc)
  (define brush (send the-brush-list find-or-create-brush "gray" 'solid))
  (define pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
  (define font (send the-font-list find-or-create-font 8 'default 'normal 'normal))
  (define-values (dc-width dc-height) (send dc get-size))
  (define cell-width (/ dc-width 8))
  (define cell-height (/ dc-height 8))
  (define margin 3)
    
  (send dc clear)
  (send dc set-brush brush)
  (send dc set-pen pen)
  (send dc set-font font)
  
  (for* ([row (in-range 8)] [col (in-range 8)]
         #:when (or (and (odd? row) (even? col))
                    (and (even? row) (odd? col))))
    (define-values [x y] (values (* col cell-width) (* row cell-height)))
    (send dc draw-rectangle x y cell-width cell-height))

  (for ([(rank index) (in-indexed '("8" "7" "6" "5" "4" "3" "2" "1"))])
    (define-values [_0 h _1 _2] (send dc get-text-extent rank font #t))
    (define y (+ (* index cell-height) (- (/ cell-height 2) (/ h 2))))
    (send dc draw-text rank margin y))
  
  (for ([(file index) (in-indexed '("a" "b" "c" "d" "e" "f" "g" "h"))])
    (define-values [w h _1 _2] (send dc get-text-extent file font #t))
    (define x (+ (* index cell-width) (- (/ cell-width 2) (/ w 2))))
    (send dc draw-text file x (- dc-height h margin))))
```

To use the code, we need to instantiate our new `chess-board%` class for the
board object, but the rest of the main program remains the same:

```racket
(define board (new chess-board%))
```

You can find the updated program in this [GitHub
Gist](https://gist.github.com/alex-hhh/3474167bcb3e19cce4923420f9818b4f) and
if you run it you will get the result as shown below.  We now have a chess
board on which to move the pieces, but the pieces can still freely move on the
board. We need to restrict the valid location of each chess piece to one of
the squares and we will address this in the next section.

![](/img/a017/chess-board2low.gif)

## Positioning the chess pieces on the board

The pasteboard and snips use a coordinate system that is based on pixels, with
the origin in the top left corner of the canvas, but a chess board is divided
in only 64 squares forming an 8x8 grid.  In addition to this, the chess game
uses a "rank" and "file" coordinate system, where the *rank* represents the
row on the board and it is numbered from 1 to 8, 1 being at the bottom, while
the *file* are the columns of the board, labeled using letters from "a" to
"h".

Since each chess piece needs to know its location, we can store it directly
into the `chess-piece%` object, as the `location` field with as setter and
getter method.  We allow the location to be `#f` which represents a piece that
is not on the board.  `make-chess-piece` will also have to be updated to allow
passing an optional location to the created chess piece

```racket
(define chess-piece%
  (class snip%
    (init-field glyph font size [location #f])

    (define/public (set-location l) (set! location l))
    (define/public (get-location) location)

    ;; rest of the chess-piece% remains the same
    ))
```

The chess pieces are added to the chess board using the `insert` method, which
allows placing a snip at any coordinate, or at (0, 0) if no coordinates are
specified.  To place a snip in its correct position, based on the location
stored inside the snip, we can derive the `pasteboard%`'s `after-insert`
method, which is invoked after each snip is inserted and call `position-piece`
for the snip:

```racket
(define chess-board%
  (class pasteboard%

    (define/augment (after-insert chess-piece . rest)
      (position-piece this chess-piece))
    
    ;; rest of the chess-board% definition remains the same
    ))

```

`position-piece` takes a chess piece, finds the `x`, `y` coordinate for it on
the chess board based on the location stored inside the snip and moves the
piece to that location using the `pasteboard%`s `move-to` method.  This
function calculates the position based on the current canvas width and height,
rather than pre-calculating the square positions, so it will work correctly
regardless of the size of the board, or even if it is invoked when the size of
the board changes:

```racket
(require embedded-gui) ; for snip-width and snip-height

(define (position-piece board piece)
  (define-values (canvas-width canvas-height)
    (let ((c (send board get-canvas)))
      (send c get-size)))
  (define-values (square-width square-height)
    (values (/ canvas-width 8) (/ canvas-height 8)))
  (define-values (rank file)
    (location->rank-file (send piece get-location)))
  (define-values (square-x square-y)
    (values (* file square-width) (* rank square-height)))
  (define piece-width (snip-width piece))
  (define piece-height (snip-height piece))
  (send board move-to piece
        (+ square-x (/ (- square-width piece-width) 2))
        (+ square-y (/ (- square-height piece-height) 2))))
```

The `location->rank-file` function, used by `position-piece`, converts a chess
board location into the row and column of the corresponding square on the
board.  It is simpler for our program to use chess board locations, written as
"d3" or "f5", as they can be read as imputs from the user or from a file:

```racket
(define (location->rank-file location)
  (unless (and (string? location) (= (string-length location) 2))
    (raise-argument-error 'location "valid chess position a1 .. h8" location))
  (define file
    (index-of '(#\a #\b #\c #\d #\e #\f #\g #\h) (string-ref location 0)))
  (define rank
    (index-of '(#\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1) (string-ref location 1)))
  (unless (and rank file)
    (raise-argument-error 'location "valid chess position a1 .. h8" location))
  (values rank file))
```

We can now write a function which loads a chess game onto the board.  The game
is encoded as a string with the piece mnemonic followed by its position.  For
example, "Ra1" means that the white rook is at square "a1".  The function just
parses the string, creates the chess pieces and inserts them onto the board.
Also, the first thing this function does is to clear the board of previous pieces, by calling the `pasteboard%`'s `clear` method:

```racket
(define initial
  (string-append
   "Ra1Nb1Bc1Qd1Ke1Bf1Ng1Rh1"
   "Pa2Pb2Pc2Pd2Pe2Pf2Pg2Ph2"
   "pa7pb7pc7pd7pe7pf7pg7ph7"
   "ra8nb8bc8qd8ke8bf8ng8rh8"))

(define (setup-board board position)
  (send board clear)
  (define piece-count (/ (string-length position) 3))
  (for ([index (in-range piece-count)])
    (define pos (* index 3))
    (define name (substring position pos (add1 pos)))
    (define location (substring position (add1 pos) (+ (add1 pos) 2)))
    (send board insert (make-chess-piece name location))))
```

If you run the program now, you will notice that, while the chess pieces are
correctly positioned initially, when the board is resized, the pieces will
stay in place, instead of moving with their respective squares.  We can fix
this by repositioning the pieces when the board is resized: the
`on-display-size` method is q called when the canvas changes size, and inside
it we can iterate over all the snips in the pasteboard and simply call
`position-piece` for each one.  Since `position-piece` looks at the location
of a piece ("d3", "f5", etc) and always takes the current canvas size into
consideration, it will move the piece at its correct location.  The code that
repositions the snip pieces is also wrapped in
`begin-edit-sequence`/`end-edit-sequence` method calls on the pasteboard.
These calls ensure that the pasteboard will not be refreshed while the snips
are moved and it will only be refreshed at the end after all snips were moved
to their new locations.  It is a good idea to wrap any code that modifies more
than one snip in a `begin-edit-sequence`/`end-edit-sequence` block, otherwise
the pasteboard will refresh more than necessary and graphics performance will
be poor.

```racket
(define chess-board%
  (class pasteboard%

    (define/augment (on-display-size)
      (send this begin-edit-sequence)
      (let loop ([snip (send this find-first-snip)])
        (when snip
          (position-piece this snip)
          (loop (send snip next))))
      (send this end-edit-sequence))
      
    ;; rest of the chess-board% definition remains the same

    ))
```

You can find the updated program in this [GitHub
Gist](https://gist.github.com/alex-hhh/96604c1a01b9ae77e5f1227536f6cb67) and
if you run it you will get the result as shown below.  While the pieces are in
place now, they can still be moved to arbitrary positions, so it is time to
start implementing some game logic now, this however is left for [another blog
post](/2018/10/chess-game-using-racket-s-pasteboard-part-2.html).

![](/img/a017/chess-board3low.gif)
