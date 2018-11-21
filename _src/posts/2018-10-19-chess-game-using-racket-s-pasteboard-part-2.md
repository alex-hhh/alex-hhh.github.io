    Title: Chess Game Using Racket's Pasteboard (part 2)
    Date: 2018-10-19T21:15:34
    Thumbnail: /img/a018/thumb.png
    Tags: racket

This is a continuation of the [previous blog
post](/2018/10/chess-game-using-racket-s-pasteboard.html), where the racket
`pasteboard%` features are explored by implementing a Chess Game Board.  In
this blog post we look at how to restrict piece movements to chess board
squares, permit only valid moves and implement turn based game play.

<!-- more -->

## Place the piece on a square after it is moved

In the last version of the chess board program could we could place any
inserted piece in its correct location, and pieces were also repositioned if
the board size changed, however, the user can still drag pieces around with
the mouse to any place on the board.  To address that, we need to override the
`after-interactive-move` method of the `pasteboard%`, which called every time
the user completes a snip drag operation.  The method receives a
`mouse-event%` object for the last event of the drag operation and this event
object contains the mouse coordinates for where the chess piece was moved to.
We can use these coordinates to determine the square location for the
destination: this is done in the `xy->location` function, as with other
functions, this function queries the size of the board, so that the location
is always determined correctly:

```racket
(define (xy->location board x y)
  (define-values (canvas-width canvas-height)
    (let ((c (send board get-canvas)))
      (send c get-size)))
  (define-values (square-width square-height)
    (values (/ canvas-width 8) (/ canvas-height 8)))
  (define-values (rank file)
    (values (exact-truncate (/ y square-height)) (exact-truncate (/ x square-width))))
  (rank-file->location rank file))
```

The `xy->location` function makes use of the `rank-file->location` function,
which constructs a chess board location from a rank and file value, its
definition is shown below:

```racket
(define (rank-file->location rank file)
  (unless (<= 0 rank 8)
    (raise-argument-error 'rank "integer between 0 and 7" rank))
  (unless (<= 0 file 8)
    (raise-argument-error 'rank "integer between 0 and 7" file))
  (string
   (list-ref '(#\a #\b #\c #\d #\e #\f #\g #\h) file)
   (list-ref '(#\8 #\7 #\6 #\5 #\4 #\3 #\2 #\1) rank)))
```

The `after-interactive-move` method finds the selected piece using
`find-next-selected-snip`, determines the location where the piece was dragged
with the mouse and tells the chess piece about it by calling `set-location`.
A call to `position-piece` will than place the piece at its new position.
`after-interactive-move` will also check if the destination square is occupied
by another piece and removes it from the board.  At this point, the code does
not check what color piece is at the destination, or that the move is actually
valid according to chess rules, this will be done later:

```racket
(define chess-board%
  (class pasteboard%

    ;; rest of the chess-board% definition remains unchanged...

    (define/augment (after-interactive-move event)
      (define piece (send this find-next-selected-snip #f))
      (define location (xy->location this (send event get-x) (send event get-y)))
      (let ((target-piece (piece-at-location this location)))
        (when (and target-piece (not (eq? piece target-piece)))
          (send target-piece set-location #f)
          (send this remove target-piece)))
      (send piece set-location location)
      (position-piece this piece))
    ))
```

The above method makes use of a `piece-at-location` function to find a chess
piece at a specified location on the chess board, this function simply
iterates over all the pieces on the board, using `get-location` to determine
if the piece is the correct one:

```racket
(define (piece-at-location board location)
  (let loop ((snip (send board find-first-snip)))
    (if snip
        (if (equal? location (send snip get-location))
            snip
            (loop (send snip next)))
        #f)))
```

----

**NOTE:** If you read the `pasteboard%` documentation, you might notice that
this class also has an `after-move-to` method, which can also be overridden.
However, this method is invoked every time a snip is moved, including as a
result of a call to the `move` method.  If snips are moved inside an
`after-move-to` method they will trigger another call to the `after-move-to`
method, and this needs to be handled carefully otherwise `after-move-to` will
be called recursively in an infinite sequence of calls.  It is best to only
override this method to implement functionality that does not require further
moving of snips.

----

You can find the updated program in this [GitHub
Gist](https://gist.github.com/alex-hhh/95530cb80d9ab81102428f5b506e2dee) and
the result of running it is shown below.  Note how the chess pieces snap to
the closest square when they are moved with the mouse and any piece at the
destination square is removed from the board.  So far, however, the program
allows any move to be made, and we will fix that, but in the next section
we'll look at how to provide better visual feedback when moving pieces.

![](/img/a018/chess-board4low.gif)

## Highlight the target square during a drag operation

Since this tutorial is about `pasteboard%` and graphics, it might be
interesting to update the previous program to highlight the destination square
as a chess piece is dragged across the board with the mouse, as this will
allow us to explore some more `pasteboard%` methods.

We will need a function which can highlight a square, the function would
simply draw another square at a specified location on the board with a
`brush%`, which is used for the background of the square, and a `pen%`, which
draws the outline.  In anticipation of further use, this method is more
complex than we strictly need now, as it allows specifying the brush and pen
colors separately, and if any of these are `#f`, the relevant part is not
drawn, therefore, to just draw the outline, you can specify #f for the
`color-name` and a valid pen color:

```racket
(define (highlight-square dc location color-name border-color-name)
  (define-values (rank file) (location->rank-file location))
  (define brush
    (if color-name
        (let* ((base (send the-color-database find-color color-name))
               (color (make-object color% (send base red) (send base green) (send base blue) 0.3)))
          (send the-brush-list find-or-create-brush color 'solid))
        (send the-brush-list find-or-create-brush "black" 'transparent)))
  (define pen
    (if border-color-name
        (send the-pen-list find-or-create-pen border-color-name 2 'solid)
        (send the-pen-list find-or-create-pen "black" 1 'transparent)))
  (send dc set-pen pen)
  (send dc set-brush brush)
  (define-values (dc-width dc-height) (send dc get-size))
  (define-values (cell-width cell-height) (values (/ dc-width 8) (/ dc-height 8)))
  (send dc draw-rectangle (* file cell-width) (* rank cell-height) cell-width cell-height))
```

Since the highlighted square is not part of the interactive section of the
`pasteboard%` it needs to be drawn as part of the `on-paint` method, together
with the chess board background.  The `on-paint` method does not know if a
mouse drag operation is in progress and where it is at, so the `chess-board%`
object will hold a `highlight-location` value, which will be managed by
methods that do have access to the mouse position and drag operation status:

```racket
(define chess-board%
  (class pasteboard%

    ;; rest of the chess-board% definition remains unchanged...

    (define highlight-location #f)

    (define/override (on-paint before? dc . other)
      (when before?
        (draw-chess-board dc)
        (when highlight-location
          (highlight-square dc highlight-location #f "indianred"))))

    ))
```

The state of the `highlight-location` variable is maintained by three methods:
`on-interactive-move`, `on-move-to`, and `after-interactive-move`.

First, we override the `on-interactive-move` method, which is called once
only, when the mouse drag operation starts, and receives the mouse event that
started the drag.  In this method, we look for the selected snip and calculate
the difference between the mouse event coordinates and the snip position --
this represents the place on the snip where the mouse picked it up.  This
information is stored in the pasteboard object as `drag-dx`, `drag-dy`
variables and it is needed because `after-interactive-move` will position the
snip in the square where the mouse pointer is, not where the top-left corner
of the dragged snip is:

```racket
(define chess-board%
  (class pasteboard%

    ;; rest of the chess-board% definition remains unchanged...
    (define drag-dx 0)
    (define drag-dy 0)

    (define/augment (on-interactive-move event)
      (define piece (send this find-next-selected-snip #f))
      (define-values (x y) (values (box 0) (box 0)))
      (send this get-snip-location piece x y #f)
      (set! drag-dx (- (send event get-x) (unbox x)))
      (set! drag-dy (- (send event get-y) (unbox y))))

    ))
```

Next, we need to override the `on-move-to` method, which is called each time a
snip is moved and it receives the `x`, `y` coordinates where the snip would be
moved to -- these coordinates represent the top left corner of the snip.  This
method is invoked even if a snip is moved as a result of a call to `move`, but
the `dragging?` flag indicates if this is a move resulting from dragging with
the mouse or not.  In this method, we can determine the location where the
chess piece would be placed by applying the `drag-dx` and `drag-dy` offsets to
the snip position and set `highlight-location` to this location.  Since the
background needs a redraw, we also queue a refresh call with the canvas.  Note
that the method implementation is careful not to call refresh if the target
location has not actually changed -- this avoids too many redraws during a
drag operation:
  
```racket
(define chess-board%
  (class pasteboard%

    ;; rest of the chess-board% definition remains unchanged...

    (define/augment (on-move-to snip x y dragging?)
      (when dragging?
        (let ((location (xy->location this (+ x drag-dx) (+ y drag-dy))))
          (unless (equal? highlight-location location)
            (set! highlight-location location)
            (send (send this get-canvas) refresh)))))

    ))
```

Finally, the `after-interactive-move` method is updated to set
`highlight-location` to `#f`, since the piece drag has now terminated and the
highlighted location does not need to be drawn anymore.

```racket
(define chess-board%
  (class pasteboard%

    ;; rest of the chess-board% definition remains unchanged...

    (define/augment (after-interactive-move event)
      ;; rest of the `after-interactive-move` definition remains unchanged...
      (set! highlight-location #f)
      (send (send this get-canvas) refresh))

    ))
```

You can find the updated program in this [GitHub
Gist](https://gist.github.com/alex-hhh/d5eca4403146d2b384712bd0cb892daa) and
the result of running it is shown below.  We will look next at how to validate
the moves and only permit valid ones on the chess board.

![](/img/a018/chess-board5low.gif)
  
## Restrict movement according to game rules

In the current state, pieces can be moved only on the board squares, but the
actual validity of a move is not considered: for example a pawn can only move
forward one square, and the user should not be able to move the piece to just
any location on the board.  We can implement this easily by adding a
`valid-moves` method to the `chess-piece%` class, which will return a list of
valid moves for the piece, than we can update the `after-interactive-move` to
check the validity of the destination location against this list.  First let's
update the `after-interactive-move` method: note that the piece location is
updated and any piece at the target location is only removed if the move is
valid, also, if the piece location is not updated, `position-piece` will
simply move the piece back to its original location.

```racket
(define chess-board%
  (class pasteboard%
  
    ;; rest of the `chess-board%` definition remains unchanged

    (define/augment (after-interactive-move event)
      (define piece (send this find-next-selected-snip #f))
      (define location (xy->location this (send event get-x) (send event get-y)))
      (define valid-moves (send piece valid-moves))
      (when (member location valid-moves) ;; NEW PART: check if a move is valid
        (send piece set-location location)
        (let ((target-piece (piece-at-location this location)))
          (when (and target-piece (not (eq? piece target-piece)))
            (send target-piece set-location #f)
            (send this remove target-piece))))
      (position-piece this piece)
      (set! highlight-location #f)
      (send (send this get-canvas) refresh))

    ))
```

### Extending `chess-piece%` to supply the list of valid moves

The `chess-piece%` object needs to have a `valid-moves` method which returns a
list of valid moves for this piece, considering the type of the piece, its
current location and the location of other pieces on the board.  Since each
piece type (king, queen, knight, etc) has a different set of moves, we could
sub-class `chess-piece%` to provide the different behavior. However, it is
actually simpler to provide stand-alone functions for each chess piece type
and supply this function to the `chess-piece%` constructor, so we extend the
constructor with a `moves` argument, which is a function taking the board and
the current location, and returning the list of moves.  We also need to add a
`color` method to the `chess-piece%` object: to determine the color we simply
look at the piece name: if it is upper case, it is a white piece, otherwise it
is black.  The overall changes to the class are quite small:

```racket
(define chess-piece%
  (class snip%
    ;; rest of the `chess-piece%` definition remains unchanged...
  
    (init-field name glyph font size moves [location #f])
    
    (define/public (color)
      (if (equal? (string-upcase name) name) 'white 'black))

    (define/public (valid-moves)
      (let ((admin (send this get-admin)))
        (if (and admin location)        ; can be #f is the snip is not owned
            (let ((board (send admin get-editor)))
              (moves board location))
            ;; Return an empty list if this piece is not on a board
            '())))
    ))
```

`make-chess-piece` can also be updated to supply the right `moves` function
for a corresponding chess piece, this is done by extending the
`chess-piece-data` hash table and just retrieving the information for it.  In
fact, the `chess-piece-data` table provides a compact way to define the things
that make each `chess-piece%` individual: its mnemonic, the glyph used for
rendering and the function used to determine its valid moves:

```racket
(define chess-piece-data
  (hash
   "K" (cons #\u2654 (king-moves 'white))
   "Q" (cons #\u2655 (queen-moves 'white))
   "R" (cons #\u2656 (rook-moves 'white))
   "B" (cons #\u2657 (bishop-moves 'white))
   "N" (cons #\u2658 (knight-moves 'white))
   "P" (cons #\u2659 (pawn-moves 'white))
   "k" (cons #\u265A (king-moves 'black))
   "q" (cons #\u265B (queen-moves 'black))
   "r" (cons #\u265C (rook-moves 'black))
   "b" (cons #\u265D (bishop-moves 'black))
   "n" (cons #\u265E (knight-moves 'black))
   "p" (cons #\u265F (pawn-moves 'black))))

(define (make-chess-piece id [location #f])
  (match-define (cons glyph moves) (hash-ref chess-piece-data id))
  (define font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
  (new chess-piece% [name id] [glyph (string glyph)] [font font]
                    [size 35] [location location] [moves moves]))
```

The last thing we need to do is implement the actual move functions, which
implement the actual game logic.  There are six move functions, one for each
type of chess piece on the board: `king-moves`, `queen-moves`, `rook-moves`,
`bishop-moves`, `knight-moves` and `pawn-moves`.  These functions are
implemented in about 100 lines of Racket code, but only the `pawn-moves` is
listed below, you can find all of them in the full listing in [this GitHub
gist](https://gist.github.com/alex-hhh/c7f3782d189482bcd73aabf272dff09c).  The
code looks complex, but it only checks various locations on the board for the
valid moves to see of they are occupied by friendly or enemy pieces:

```racket
(define (valid-rank? rank) (and (>= rank 0) (< rank 8)))
(define (valid-file? file) (and (>= file 0) (< file 8)))

(define ((pawn-moves color) board location)
  (define direction (if (eq? color 'white) -1 1))
  (define-values (rank file) (location->rank-file location))
  (define moves '())
  (when (valid-rank? (+ rank direction))
    ;; can move forward if that square is not occupied
    (let ((candidate (rank-file->location (+ rank direction) file)))
      (unless (piece-at-location board candidate)
        (set! moves (cons candidate moves))
        (when (valid-rank? (+ rank direction direction))
          ;; can move two squares forward if the pawn is in its original location
          (when (or (and (eq? color 'white) (equal? rank 6))
                    (and (eq? color 'black) (equal? rank 1)))
            (let ((candidate (rank-file->location (+ rank direction direction) file)))
              (unless (piece-at-location board candidate)
                (set! moves (cons candidate moves))))))))
    ;; can move forward left if that square is occupied
    (when (valid-file? (sub1 file))
      (let ((candidate (rank-file->location (+ rank direction) (sub1 file))))
        (let ((piece (piece-at-location board candidate)))
          (when (and piece (not (eq? color (send piece color))))
            (set! moves (cons candidate moves))))))
    ;; can move forward right if that square is occupied
    (when (valid-file? (add1 file))
      (let ((candidate (rank-file->location (+ rank direction) (add1 file))))
        (let ((piece (piece-at-location board candidate)))
          (when (and piece (not (eq? color (send piece color))))
            (set! moves (cons candidate moves)))))))

  moves)
```

### Highlight valid moves and opponent controlled squares

The `valid-moves` method can also be used to highlight squares that are
controlled by the opponent as well as the valid moves for the selected piece.
Just as with highlighting the current location, the actual drawing of the
controlled squares is done in the `on-paint` method, and since this method has
no access to the game logic itself, it relies on two lists of locations
`valid-move-locations` for the location where the selected piece can move and
`opponent-move-locations` where the opponent pieces can move to.  The
`after-select` method is called when a piece is selected or unselected and we
can override this method to calculate the valid moves lists.  The
`after-interactive-move` also needs to keep track of the
`valid-move-locations`, since the location of the selected piece changes after
a move:

```racket
(define chess-board%
  (class pasteboard%
    ;; rest of the chess-board% definition remains unchanged
  
    (define valid-move-locations '())
    (define opponent-move-locations '())

    (define/override (on-paint before? dc . other)
      (when before?
        (draw-chess-board dc)
        (for ((location (in-list valid-move-locations)))
          (highlight-square dc location #f "seagreen"))
        (for ((location (in-list opponent-move-locations)))
          (highlight-square dc location "firebrick" #f))
        (when highlight-location
          (highlight-square dc highlight-location #f "indianred"))))
          
    (define/augment (after-select snip on?)
      (if on?
          (begin
            (set! valid-move-locations (send snip valid-moves))
            (set! opponent-move-locations
                  (collect-opponent-moves this (send snip color))))
          (begin
            (set! opponent-move-locations '())
            (set! valid-move-locations '())))
      (send (send this get-canvas) refresh))
    
    (define/augment (after-interactive-move event)
      ;; rest of the `after-interactive-move` definition remains unchanged
      
      ;; Note: piece is still selected, but the valid moves are relative to
      ;; the new position
      (set! valid-move-locations (send piece valid-moves))
      (send (send this get-canvas) refresh))

    ))
```

The `collect-opponent-moves` function is used by `after-select` to get the
list of all possible move locations that can be made by the opponent color.
The function simply collects all valid moves from the opponent pieces and
removes duplicates from them:

```racket
(define (collect-opponent-moves board color)
  (define moves '())
  (let loop ((snip (send board find-first-snip)))
    (when snip
      (unless (eq? (send snip color) color)
        (set! moves (append moves (send snip valid-moves))))
      (loop (send snip next))))
  (remove-duplicates moves))
```

You can find the updated program in this [GitHub
gist](https://gist.github.com/alex-hhh/c7f3782d189482bcd73aabf272dff09c) and
the result of running it is shown below.  We will look next at how to
implement turn based game play.

![](/img/a018/chess-board6low.gif)

## Turn based game play

The last thing we need to implement a proper chess game is to keep track of
which color has to move next, as currently, any color piece can be moved at
any time.  We will also update the `pasteboard%` to display a message if the
user tries to move a piece of the wrong color.

We'll start by adding support to display a message on top of the game play
board.  The message is not interactive, so it will be drawn from the
`on-paint` function, however, since the message needs to be drawn on top of
the chess pieces, it will be drawn on the second invocation of `on-paint`,
when the `before?` argument is `#f`.  The message is stored in the `message`
field and is updated by the `set-message` method, this method also sets up a
timer to clear the message after a predefined interval (2 seconds in our case)
and also requests a canvas refresh, as without that, the `pasteboard%` does
not know that it needs a redraw:

```racket
(define chess-board%
  (class pasteboard%
    ;; Rest of the chess-board% definition remains unchnaged...

    (define message #f)
    (define message-timer
      (new timer%
           [notify-callback (lambda ()
                              (set! message #f)
                              (send (send this get-canvas) refresh))]))
                              
    (define (set-message m)
      (set! message m)
      (send message-timer start 2000)
      (send (send this get-canvas) refresh))

    (define/override (on-paint before? dc . other)
      (if before?
          (begin
            (draw-chess-board dc)
            (for ((location (in-list valid-move-locations)))
              (highlight-square dc location #f "seagreen"))
            (for ((location (in-list opponent-move-locations)))
              (highlight-square dc location "firebrick" #f))
            (when highlight-location
              (highlight-square dc highlight-location #f "indianred")))
          (when message ;; NEW PART: show a message, if any, on top
            (display-message dc message))))
    
    ))
```

The actual drawing of the message is done in the `display-message` function,
which is shown below.  The function calculates the text position such that the
message is always displayed in the middle of the board:

```racket
(define (display-message dc message)
  (define font (send the-font-list find-or-create-font 24 'default 'normal 'normal))
  (define-values [w h _1 _2] (send dc get-text-extent message font #t))
  (define-values (dc-width dc-height) (send dc get-size))
  (define-values (x y) (values (/ (- dc-width w) 2) (/ (- dc-height h) 2)))

  (define brush (send the-brush-list find-or-create-brush "bisque" 'solid))
  (define pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
  (send dc set-brush brush)
  (send dc set-pen pen)
  (send dc draw-rectangle 0 y dc-width h)
  (send dc set-font font)
  (send dc set-text-foreground "firebrick")
  (send dc draw-text message x y))
```

To maintain the color that can move next, we can use a `turn` field to store
the piece color `'white'` or `'black`.  The `can-interactive-move?` method is
invoked by the `pasteboard%` to check if a snip can be moved with the
mouse. We can override this method to return `#t` if the selected piece color
is the same as the value of `turn`, and `#f` otherwise.  This method also
calls `set-message` to display a message if the user tries to move a piece of
the wrong color.  The `after-interactive-move` also needs to be updated to
alternate the colors for `turn` after a successful move.  As a help for the
user, the `after-select` method is also updated to display a message if the
user selects a piece of the wrong color, however the selection is still
allowed, so the valid moves squares are still highlighted -- even though the
wrong color piece can be selected, it will not move as `can-interactive-move?`
will prevent that:

```racket
(define chess-board%
  (class pasteboard%
    ;; Rest of the chess-board% definition remains unchnaged...

    (define turn 'white)
    
    (define/augment (can-interactive-move? event)
      (define piece (send this find-next-selected-snip #f))
      (unless (eq? turn (send piece color))
        (set-message (format "It's ~a turn to move"
                      (if (eq? turn 'white) "white's" "black's"))))
      (eq? turn (send piece color)))

    (define/augment (after-interactive-move event)
      ;; rest of `after-interactive-move` definiton remains unchanged...
      (when (member location valid-moves)
        ;; rest of the lines unchanged...
        (set! turn (if (eq? turn 'white) 'black 'white))))

    (define/augment (after-select snip on?)
      (if on?
          (begin
            ;; rest of the lines unchanged...
            (unless (eq? turn (send snip color))
              (set-message (format "It's ~a turn to move"
                            (if (eq? turn 'white) "white's" "black's")))))
          ;; rest of the lines unchanged...
          ))
    ))
```

You can find the updated program in this [GitHub
gist](https://gist.github.com/alex-hhh/f6ea1a5cba92914272f1ce8c66e55455) and
the result of running it is shown below.  While this program is significantly
more complex than what we started with, it is still only 440 lines of code and
it implements an almost complete chess game play -- there are a few rules
missing, such as castling and en-passant capture, but since they don't
contribute anything to learning about Racket `pasteboard%` objects, they were
left out, feel free to take the program and extend it.

![](/img/a018/chess-board7low.gif)

If you try to use the program for a bit, you might notice that the
`pasteboard%` implements behavior that is not useful in a chess game: one can
select and move multiple pieces, move pieces with the keyboard or deleting
pieces from the board by pressing delete.  In the next post, we'll look at how
to fix these problems.

