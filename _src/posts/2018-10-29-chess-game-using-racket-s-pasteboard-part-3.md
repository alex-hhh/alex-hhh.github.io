    Title: Chess Game Using Racket's Pasteboard (part 3)
    Date: 2018-10-29T18:34:41
    Thumbnail: /img/a019/thumb.png
    Tags: racket

The `pasteboard%`object is an editor of `snip%` objects and it implements some
features that make sense for an editor: for example, you can select multiple
snips and drag them around with the mouse, and you can move selected snips
using the keyboard, you can also add any kind of snip, not just chess pieces
to the pasteboard.  Since none of these features are useful or desirable for a
chess board game we will look at how to disable them.

<!-- more -->

This is the last post in the series about implementing a chess board game
using the Racket `pasteboard%`.  If you haven't already done so, you might
want to read the [first blog
post](/2018/10/chess-game-using-racket-s-pasteboard.html) and [the second
one](/2018/10/chess-game-using-racket-s-pasteboard-part-2.html) before reading
this one.

## Selecting only one chess piece at a time

By default, the `pasteboard%` will allow multiple snips to be selected and
this can be done in two ways: dragging the mouse over a region of the canvas
will select all snips in that region, and holding down shift while clicking on
a snip will not remove the selection from the previous snip.  This behavior is
not suitable for a chess game, so we need to disable both ways of performing a
multiple selection:

* disabling drag selection is done by calling `set-area-selectable` with an
  `#f` argument
* disabling multiple selection with the Shift key is done by overriding the
  `after-select` method and un-selecting any other snips when a snip is
  selected.

```racket
(define chess-board%
  (class pasteboard%
    ;; rest of the chess-board% definition remains unchanged...

    (send this set-area-selectable #f) ; disable drag-select for the pasteboard

    (define/augment (after-select snip on?)
      (when on?
        (define other-selected-snips
          (let loop ((other (send this find-next-selected-snip #f))
                     (result '()))
            (if other
                (let ((next (send this find-next-selected-snip other)))
                  (if (eq? snip other)
                      (loop next result)
                      (loop next (cons other result))))
                result)))
        (for ([snip other-selected-snips])
          (send this remove-selected snip)))
      ;; Rest of the after-select definition remains unchanged...
      )

    ))
```

It is worth mentioning two things about the code that un-selects snips in
`after-select`:

* since selected snips are linked together using `find-next-selected-snip`, we
  cannot immediately remove a selected snip without breaking that chain, so
  the code collects all selected snips in the `other-selected-snips` list and
  removes them separately.  Technically, there will only be at most one other
  selected snip, but the code is written such that it can un-select any number
  of snips.
* Each time we remove a snip, the `after-select` method will be called for
  that snip with the `on?` parameter set to `#f`, the code needs to be
  prepared to handle these calls.  Our code could be affected by this since it
  sets the `opponent-move-locations` and `valid-move-locations` to the empty
  list when a snip is unselected -- however since we set the move locations
  last for our snip, they will always be set for the selected snip.

## Moving the snip in front when dragging it

The snips are stored in the `pasteboard%` in a list, in the same order as they
are added and they are also drawn on the canvas in this order.  This drawing
order is kept even when a snip is dragged with the mouse and this means that
the snip will appear to drag under chess pieces which were added to the
`pasteboard%` before this snip.  To drag it above all other pieces, we need to
move it to the front of the snip list.

We need to update `after-select` (again) to call `set-before` and place the
selected chess piece in the front of the list.  Our application does not rely
on the order in which snips are stored in the `pasteboard%` so we don't need
to do place the snip back in its place after it was moved.

```racket
(define chess-board%
  (class pasteboard%
    ;; rest of the chess-board% definition remains unchanged...

    (define/augment (after-select snip on?)
      (when on?
        (send this set-before snip #f)
        ;; Rest of the after-select definition remains unchanged...
      ))
    ))
```

## Disable `pasteboard%` edits with the keyboard and clipboard

The `pasteboard%` is configured as an editor and, as such, it is integrated
with the keyboard and the clipboard.  For example, you can select a snip than
move it with the arrow keys or delete it by pressing Del or Backspace.  If the
application would have an edit menu, you could also Cut, Copy and Paste these
snips.  All this is undesired behavior in a chess game and we need to disable
it.

To disable the Cut/Copy/Paste operations, we need to override the
`can-do-edit-operation?` method to always return `#f`.  This method is invoked
to check if the pasteboard is ready to accept a CUT, COPY, PASTE or some other
edit operation where the operation name is passed in as an argument. Since we
won't support any, we will simply return `#f` in this method.

To disable the keyboard, we need to define a new `keymap%` instance mapping
the unwanted keys to an "ignore" function.  This keymap is than installed in
the `pasteboard%` before the internal keymap so it effectively overrides these
keys.  Note that we could have implemented the "up", "down", "left" and
"right" keys to move the piece square by square, by this is left as an
exercise to the reader, instead we map them to the "ignore" function which is
defined as a function that displays a message informing the user that the key
is disabled -- in a real program, this function would simply do nothing, but
displaying a message is more useful in a demo program.

```racket
(define chess-board%
  (class pasteboard%
    ;; Rest of the chess-board% definition remains unchanged...

    (define/override (can-do-edit-operation? op recursive?)
      #f)
      
    (define (on-disabled-key-event data event)
      (if (is-a? event key-event%)
          (let* ((code (send event get-key-code))
                 (key-name (cond ((symbol? code) (symbol->string code))
                                 ((equal? code #\backspace) "backspace")
                                 ((equal? code #\rubout) "delete")
                                 ((equal? code #\space) "space")
                                 ((equal? code #\return) "return")
                                 (#t (string code)))))
            (set-message (format "~a key is disabled" key-name)))
            (set-message "event is discarded")))

    (define k (new keymap%))
    (send k add-function "ignore" on-disabled-key-event)
    (send k map-function "up" "ignore")
    (send k map-function "down" "ignore")
    (send k map-function "left" "ignore")
    (send k map-function "right" "ignore")
    (send k map-function "del" "ignore")
    (send k map-function "backspace" "ignore")
    (send this set-keymap k)
  ))
```

We could have disabled the unwanted keyboard events by overriding the
`on-char` method -- the `on-default-char` method is not useful for this
purpose, as it is invoked only if the key press is not processed by a keymap.
However, the `on-char` method performs some other functions and overriding it
correctly is tricky and it is easy to get it wrong and break `pasteboard%`
functionality.  It is simpler and safer to use keymaps for this feature.

## Changing the way selected chess pieces are highlighted

By default, the `pasteboard%` object will draw eight small squares around
selected snips (you can see this selection in all the examples in this and
previous blog posts), however, we can change the way selection looks.

The drawing of the squares around selected snips can be disabled using the
`set-selection-visible` method, but this will not prevent selecting snips --
snips are still selected, however there is no visual cue to this.  We can
however change the `chess-piece%` class to know that it is selected and draw
itself in a different way -- to keep this simple, the code is updated to draw
selected pieces in red.  A new flag `selected?` is added to this class and it
can be set using the `set-selected` method, than, the `draw` method is updated
to change the text foreground color to red if the chess piece is selected.
The `set-selected` method will also need to inform the snip administrator that
this snip has changed the way it is displayed and needs to be re-drawn, this
is done using the `needs-update` method:

```racket
(define chess-piece%
  (class snip%
    ;; Rest of the `chess-piece%` definition remains unchanged...
    (define selected? #f)

    (define/public (set-selected on?)
      (set! selected? on?)
      (let ((admin (send this get-admin)))
        (when admin
          (send admin needs-update this 0 0 size size))))

    (define/override (draw dc x y . other)
      (send dc set-font font)
      (if selected?
          (send dc set-text-foreground "red")
          (send dc set-text-foreground "black"))
      ;; Rest of the drawing code remains unchanged...
      )
    ))
```

The `chess-board%` definition is updated to disable the visible selection by
calling `set-selection-visible` with an `#f` argument, and the `after-select`
method is updated to inform the chess piece snip whether it was selected or
unselected.

```racket
(define chess-board%
  (class pasteboard%
    ;; Rest of the chess-board% definition remains unchanged
    (send this set-selection-visible #f)

    (define/augment (after-select snip on?)
      (send snip set-selected on?)
      ;; rest of the `after-select` definition remains unchanged...
      )
    ))
```

## Validating chess pieces as they are added to the board

The code, as written so far, has a lot of checks to ensure that the board
remains in a consistent state when the user interacts with it, however, as a
programmer, one can still create inconsistencies, for example by inserting two
chess pieces at the same locations, or by trying to add some snips that are
not chess pieces.  We can prevent that by overriding the `can-insert?` method
of the `pasteboard%` which will be called to validate a snip before it is
inserted: if this method returns `#f` the snip will not be inserted in the
pasteboard.  On our case, we will check if the snip is an instance of
`chess-piece%`, if it has a valid location and if that location is available
on the board:

```racket
(define chess-board%
  (class pasteboard%
    ;; rest of the `chess-board%` definition remains unchanged...

    (define/augment (can-insert? snip . rest)
      (and                             ; We can insert a snip if...
       (is-a? snip chess-piece%)       ; ... it is an instance of chess-piece%
       (send snip get-location)        ; ... has a location 
       (not (piece-at-location this (send snip get-location))))) ;; ... that location is empty
    ))
```

The `pasteboard%` will not report an error if the program tries to insert a
snip and `can-insert?` returns false, instead, the snip is simply not added to
the `pasteboard%`.  To check if an insert succeed or not, you can call the
`is-owned?` method on the snip itself after the insert call, the method will
return `#t` of the snip was inserted, and therefore now owned by the
`pasteboard%` and `#f` if the snip was not inserted.


## The final chessboard program

You can find the final version of the program in this [GitHub
Gist](https://gist.github.com/alex-hhh/4817c4d0353e40b72108e7e753c3d0da), the
entire program, including comments is under 800 lines of code, and it has a
lot of functionality for such a small program.  In addition to that, the
program does not use any external libraries: you can simply install
[Racket](https://racket-lang.org/) and run the program in DrRacket directly --
it will also run on Linux, Mac OS and Windows without any modifications.

![](/img/a019/chess-board8low.gif)

This is not a complete program, and it is missing several features from being
a complete chess game, but it is really intended to be used for exploring
Racket GUI programming and pasteboard% use in particular, so feel free to
copy, extend or otherwise experiment with this program.
