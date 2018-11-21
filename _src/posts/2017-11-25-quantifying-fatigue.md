    Title: Quantifying Fatigue
    Date: 2017-11-25T09:07:30
    Tags: training data analysis
    Thumbnail: /img/a004/thumb.png

I needed a way to measure fatigue as it accumulates during a running session.
There is no actual "fatigue" measurement that I'm aware of, so I had to find a
substitute.

<!-- more -->

Intuitively, the longer one runs, the more tired he will become, but using
time as a proxy for fatigue does not take into account that one hour of easy
running does not produce the same fatigue as one hour of hard interval
training.  Instead, I decided to use the number of heart beats from the start
of the session as a proxy for fatigue: the more heart beats are accumulated,
the more tired you are.  Since heart rate can never go negative, the heart
beats value is always growing.  Heart beats also accounts for the intensity of
the effort, meaning that more of them are accumulated in the same amount of
time during hard interval training versus an easy run.

To show an example, below are two runs of approximately the same duration (1
hour 20 minutes), one easy run and one hard interval run:

![](/img/a004/hr-comparison.svg)

The heart beats value accumulate at different rates for the two runs, as shown
below:

![](/img/a004/hb-comparison.svg)

To make it easier to work with "fatigue" values, I decided to "split" the
heartbeat values into ranges.  I decided to split fatigue levels at 30 minute
sections at 160 BPM heart rate.  Why 160 BPM?  For me, this is the top of
"Zone 2", that is the top of "easy running" zone.  Why 30 minutes? Again, for
me, I can run 30 minutes at an easy pace with no perceived fatigue at all.

The table below summarizes the fatigue values and the heartbeat ranges for
each of them.  I have also added a "Data Points" column representing the
number of data points at each fatigue value for my 2017 Marathon training
season.  Most of the data points are in the "none" fatigue level, and the
least of the points are in the "severe" fatigue level.  This is expected,
since it takes more than 2 hours of easy running to get to severe fatigue, and
most of my runs were less than that.

|Fatigue Level| Description | Time @160 BPM     | Heart Beats   | Data Points |
|:-----------:|-------------|-------------------|---------------|------------:|
| 1           | none        | 0 - 30 minutes    | 0 - 4800      | 179182      |
| 2           | mild        | 30 - 60 minutes   | 4801 - 9600   | 139689      |
| 3           | moderate    | 60 - 90 minutes   | 9601 - 14400  | 72519       |
| 4           | heavy       | 90 - 120 minutes  | 14401 - 19200 | 31306       |
| 5           | severe      | above 120 minutes | above 19200   | 15384       |

## Technicalities

The function shown below will add the heart beats series to a session, which
in [ActivityLog2][al2-link] is represented as a [data-frame%][df-link] object.
The series that already exist for a session, such as "elapsed" and "hr", are
documented [here][session-df-link]:

```racket
(define (add-heartbeats-series df)

  (define hb 0)                         ; holds accumulated heart beats

  ;; Called by the `data-frame%/map` method with two adjacent data points,
  ;; PREV and NEXT. The value returned by this function will be the data point
  ;; in the new series at the same index as NEXT
  (define (accumulate-heart-beats prev next)

    (when prev                          ; first time PREV is #f
      (match-let (((vector e1 hr1) prev)
                  ((vector e2 hr2) next))
        (when (and e1 hr1 e2 hr2)
          (let ((beats (/ (* 0.5 (+ hr1 hr2)) 60.0)))
            (set! hb (+ hb (* beats (- e2 e1))))))))
    
    (exact-truncate hb))
  
  (unless (send df contains? "hb")  ; might already be present due to caching!
    (send df add-derived-series
          "hb"                          ; new series name
          '("elapsed" "hr")             ; based on these series
          accumulate-heart-beats        ; generator function for the new series
          )))
```

The functions below, are used to convert a heartbeats value into a "fatigue"
value, which is a number between 1 and 5:

```racket
(define (heartbeats->fatigue hb)
  ;; 30 min @ 160bpm (z2 lower limit) increments, max out at 5.
  (exact-truncate (min (+ 1 (/ hb (* 30 160))) 5.0)))

(define (fatigue-name f)
  (cond ((<= f 1.0) "no fatigue")
        ((<= f 2.0) "mild fatigue")
        ((<= f 3.0) "moderate fatigue")
        ((<= f 4.0) "heavy fatigue")
        (#t "severe fatigue")))
```

The source code for generating the plots on this page is available [here](https://gist.github.com/alex-hhh/204be1d29e518689c9621ec9cdb5f4f2).

[al2-link]: https://github.com/alex-hhh/ActivityLog2
[df-link]: https://github.com/alex-hhh/ActivityLog2/blob/master/doc/data-frame.md
[session-df-link]: https://github.com/alex-hhh/ActivityLog2/blob/master/doc/session-df.md
