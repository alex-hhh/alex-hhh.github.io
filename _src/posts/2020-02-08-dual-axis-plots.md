    Title: Dual Axis Plots
    Date: 2020-02-08T07:10:18
    Thumbnail: /img/a034/thumb.png
    Tags: racket

... in which we explore how to show on the same plot two data series that have
different data ranges.  The [Racket Plot][plot] package does not support this
functionality directly, but with the help of some data transformation, we can
still achieve good results.

<!-- more -->

The plot below shows the elevation and grade plotted against distance for a
hiking activity.  Elevation values range between 850 and 1300 meters, and the
grade values range from -30% to about 30%, so simply plotting them together
would not produce a nice looking result.  Instead the data needs to be
transformed and some extra plot parameters need to be used to show a correct
scale for both data series.

![](/img/a034/grade-elevation.svg)

This blog post aims to illustrate a general mechanism for constructing dual
axis plots, but since it is difficult to have an abstract discussion, all the
examples will show how to plot elevation and grade (slope) data from a hiking
activity.  However, the same idea can be used for any type of data.

## Limitations of the Plot Package

So you have some data recorded for a hiking activity and you would like to
plot the elevation profile as well as the grade along the route.  The task is
simple enough: just load the data in a [data frame][data-frame] and use the
plot package to display it:

```racket
(define df (df-read/csv "./track-data-1858.csv"))

(list
  (parameterize ([plot-x-label "Distance (km)"]
                 [plot-y-label "Grade (%)"])
    (plot (lines (df-select* df "distance" "grade"))))
  (parameterize ([plot-x-label "Distance (km)"]
                 [plot-y-label "Elevation (meters)"])
    (plot (lines (df-select* df "distance" "calt")))))
```

Which will produce two plots side-by-side:

![](/img/a034/grade-elevation-separate.svg)

How can we combine these two plots into a single one?  After all, both the
elevation and grade data samples are from the same hiking activity and they
share the same X axis (distance, in this case).  Well, the plot package allows
rendering multiple data sets on the same plot, and it does allow defining a
"far Y axis", which is the axis on the right of the plot.  So, in the first
try, we could write:

```racket
(parameterize ([plot-x-label "Distance (km)"]
               [plot-y-label "Grade (%)"]
               [plot-y-far-label "Elevation (meters)"])
  (plot (list
         (lines (df-select* df "distance" "grade"))
         (lines (df-select* df "distance" "calt")))))
```

Unfortunately, this does not produce a good result: the elevation data series
shows up relatively OK on the plot, but the grade series is barely visible as
it hovers around the 0 axis.  It does make sense, however: there is a single X
axis (distance), but also a single Y axis, which shows both elevation and
grade.  Since elevation values range between 850 and 1300 meters, and the
grade values range from -30% to about 30%, the Y axis will display a data
range from -30 to about 1300.  This range represents about 33% of the
elevation data range, but only about 4% of the grade data range, so neither
plots will occupy the entire Y range and the grade plot, using only 4% of the
plot area, will be practically useless.

![](/img/a034/grade-elevation-combined.svg)

## Dual Axis Plot

The Racket plot package is only able to use a single Y axis for the plot area,
and our data series use separate ranges -- [-30 .. 30] for the grade data and
[850 .. 1300] for elevation -- so we will need to transform one of the data
series to have the same data range before plotting them.

What is the range of our data series anyway?  The data-frame package provides
a `df-statistics` function which returns a [statistics][racket-statistics]
object about a data series.  This statistics object contains information about
the minimum, maximum, average values of the data (plus a few more things).
Since we are interested in only the minimum and maximum values for a data
series, we will write a helper function, `get-low+high`, which returns these
two values, adjusted to allow for some room at the top and bottom of the plot:

```racket
(define (get-low+high df series [room 0.25])
  (let* ([stats (df-statistics df series)]
         [low (statistics-min stats)]
         [high (statistics-max stats)]
         [adjust (* (statistics-range stats) room)])
    ;; Make the Y range of the plot slightly larger than 
    ;; the min/max values
    (values (- low adjust) (+ high adjust))))
```

The `get-low+high` function can be tested directly in DrRacket, to check that
it produces reasonable values for our data series.  Note that the function
does not return the minimum and maximum values of the series, but a slightly
larger interval, so there will be an "inset" inside the plot, and the plotted
lines will not touch the plot borders.

```racket
> (get-low+high df "grade")
-46.699999999999996
48.099999999999994
> (get-low+high df "calt")
749.0727499999999
1384.21025
```

The next step is to write a function which transforms values from one data
range to another.  We could put the above numbers directly in the transform,
but it is useful to create a more general function, which takes the two ranges
as parameters and constructs the transform:

```racket
(define (make-g->b base-min base-max guest-min guest-max)
  (define base-range (- base-max base-min))
  (define guest-range (- guest-max guest-min))
  (lambda (v)
    (let ([p (/ (- v guest-min) guest-range)])
      (+ base-min (* p base-range)))))
```

And here is another helper function which transforms an entire data set from
one range to another:

```racket
(define (g->b data transform)
  (for/vector ([item data])
    (match-define (vector x y) item)
    (vector x (transform y))))
```

With the data transformation routines in place, we can now create a plot where
we transform one data set to use the data range of another.  In the example
below, the altitude data series is transformed into the range of the grade
series, but this choice is arbitrary, and we could have equally well
transformed the grade series into the range of he elevation series with the
same result.  To do that, the arguments to `make-g->b` would have to be
swapped.

```racket
(let-values ([(alt-min alt-max) (get-low+high df "calt")]
             [(grade-min grade-max) (get-low+high df "grade")])
  (define transform (make-g->b grade-min grade-max alt-min alt-max))
  (parameterize ([plot-x-label "Distance (km)"]
                 [plot-y-label "Grade (%)"]
                 [plot-y-far-label "Elevation (meters)"])
    (plot-pict (list
                (lines (df-select* df "distance" "grade"))
                (lines (g->b (df-select* df "distance" "calt") transform))))))
```

... and the result is more pleasing:

![](/img/a034/grade-elevation-1.svg)

## Drawing Labels for Both Data Series 

The two data series now occupy the entire plot area, but the "guest" series,
which in our case is the elevation, has no labels drawn on the right hand
axis, creating the appearance that the elevation also ranges from -30 to 30.
The [plot-y-far-ticks][plot-y-far-ticks] plot parameter can be used to specify
the ticks to be drawn, but we will need to scale them as well in order to
display values in the appropriate range, which can be done using
[ticks-scale][ticks-scale].

The [ticks-scale][ticks-scale] function requires two parameters: the plot
ticks to scale, here we can use [linear-ticks][linear-ticks] which are the
default, and something called an [invertible-function][invertible-function].

[invertible-function][invertible-function] is a structure containing two
functions used to convert from one range to another and back.  The `make-g->b`
function already creates one of these functions, the one which transforms a
"guest" data range into a "base" data range, and we will need to define the
reverse one which transforms the "base" data range into the "guest" data
range.  The two functions are similar, with just the parameters reversed, and
to avoid confusion, we'll create the invertible-function in
`make-guest-transform` which will replace `make-g->b`:

```racket
(define (make-guest-transform base-min base-max guest-min guest-max)
  (define base-range (- base-max base-min))
  (define guest-range (- guest-max guest-min))
  (invertible-function
   (lambda (v)                          ; Transform from base to guest
     (let ([p (/ (- v base-min) base-range)])
       (+ guest-min (* p guest-range))))
   (lambda (v)                          ; Transform from guest to base
     (let ([p (/ (- v guest-min) guest-range)])
       (+ base-min (* p base-range))))))
```

The invertible function structure contains one of the function to transform
the input data, so we can use the result of `make-guest-transform` to
`guest->base` which does the data transform (again, to avoid confusion, we
didn't update `g->b`):

```racket
(define (guest->base guest-data transform)
  (let ([tr (invertible-function-g transform)])
    (for/vector ([item guest-data])
      (match-define (vector x y) item)
      (vector x (tr y)))))
```

With all this preparation, we can now plot the altitude and grade data series
on the same plot, with an Y axis on each side, the left one corresponding to
grade data and the right one to the elevation data:

```racket
(let-values ([(alt-min alt-max) (get-low+high df "calt")]
             [(grade-min grade-max) (get-low+high df "grade")])
  (define transform (make-guest-transform grade-min grade-max alt-min alt-max))
  (parameterize ([plot-x-label "Distance (km)"]
                 [plot-y-far-label "Elevation (meters)"]
                 [plot-y-label "Grade (%)"]
                 [plot-y-far-ticks (ticks-scale (linear-ticks) transform)])
    (plot (list
           (lines (df-select* df "distance" "grade"))
           (lines (guest->base (df-select* df "distance" "calt") transform))))))
```

which produces the following plot:

![](/img/a034/grade-elevation.svg)

## Final thoughts

Can this technique be extended for more than two data series?  Well, yes and
no: any number of data series can be transformed into the base data range by
creating individual transforms, but the problem is that the Racket plot
package only allows two Y axis on the plot, left and right (also called near
and far axis), and this means that no axis can be displayed for the additional
data series.  In fact, this blog post is inspired by a [Racket Plot Package
Issue][plot-issue], which illustrates this limitation of the plot package.

[plot-issue]: https://github.com/racket/plot/issues/25
[data-frame]: /2018/08/racket-data-frame.html
[racket-statistics]: https://docs.racket-lang.org/math/stats.html#(def._((lib._math%2Fstatistics..rkt)._statistics))
[plot-y-far-ticks]: https://docs.racket-lang.org/plot/ticks_and_transforms.html?#(def._((lib._plot%2Fmain..rkt)._plot-y-far-ticks))
[ticks-scale]: https://docs.racket-lang.org/plot/ticks_and_transforms.html#(def._((lib._plot%2Fmain..rkt)._ticks-scale))
[invertible-function]: https://docs.racket-lang.org/plot/ticks_and_transforms.html#%28def._%28%28lib._plot%2Fmain..rkt%29._invertible-function~3f%29%29
[plot]: https://docs.racket-lang.org/plot/index.html
[invertible-function]: https://docs.racket-lang.org/plot/ticks_and_transforms.html#(def._((lib._plot%2Fmain..rkt)._invertible-function~3f))
[linear-ticks]: https://docs.racket-lang.org/plot/ticks_and_transforms.html#%28def._%28%28lib._plot%2Fmain..rkt%29._time-ticks%29%29
