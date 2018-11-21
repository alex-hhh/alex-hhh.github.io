    Title: Fatigue and Running Form
    Date: 2017-11-28T19:06:57
    Tags: training data analysis
    Thumbnail: /img/a005/thumb.png

My Garmin FR920 watch keeps track of *running form* related measurements:
Cadence, Stride, Ground Contact Time and Vertical Oscillation.  Given that I
collected a lot of data during my last Marathon training, I decided to have a
look at how running form changes as I get more tired during a run.

<!-- more -->

During my last Marathon training season, my watch recorded 440179 data points
from 97 running sessions for a total distance of 1347 km or 122 hours of
running.  Some of these are long and easy runs and some are interval training
runs -- there is a large variety of running paces at different fatigue levels.
I wrote another [blog post](../2017/marathon-training-2017-statistics.html)
about some statistics from this data, but this post focuses on fatigue and how
it affects Cadence, Stride, Ground Contact Time and Vertical Oscillation.

I wrote a separate post about how I decided to [quantify
fatigue](../2017/quantifying-fatigue.html) accumulated during a run workout.
The plots in this post use that method to group data points into five fatigue
levels.

## Cadence + Stride vs Fatigue

Running pace (how fast you run), is composed of two elements: **cadence** --
how many steps one takes each minute and **stride** -- how long is each step.
The plot below plots the cadence vs. stride for each data point, the points
being colored by fatigue.  Each point also represents a running pace value,
The lines on the plot represent the "constant pace" lines: pace is "stride"
(the length of each step) multiplied by "cadence" (how many steps each
minute).  For example, to maintain a 4:00 min/km pace at a cadence of 90
steps/minute, the stride would have to be 1.38 meters.  To maintain the same
pace at a cadence of 80 steps/minute, the stride would have to increase to
1.56 meters.

![](/img/a005/cad+stride-fatigue.svg)

*A note on cadence values:* Originally, running cadence was measured by
counting number of steps taken with one leg, either left or right.  A few
years ago, Garmin switched to measuring cadence by counting steps from both
legs.  I prefer the old style, where a cadence of 90 means "90 steps taken
with the right (or left) leg only".  If you prefer the newer style, just
multiply everything by 2. For example, an "old style" 90 SPM becomes 180 SPM
when counting each leg.

## Density plots for each running form metric

The plots below show how the density profile (a histogram, really) for each of
the running metrics.  Ground Contact Time, or GCT, is the time the foot is in
contact with the ground at each step.  Vertical Oscillation, or VOSC, is the
amount of up-down bouncing the body makes while running.  It is measured in
millimeters.

The cadence, GCT and VOSC density profiles do not seem change much during the
course of a run.  There are some humps in the plot mainly because I did
intervals at hard and severe fatigue levels and I maintain a higher cadence
during these.

Stride (the length of each step) is the only measurement that shows a clear
shift towards lower values as fatigue increases.

----

![](/img/a005/cad-density.svg)

| Fatigue Level | Mean Cadence (SPM) | StdDev |
|---------------|--------------------|--------|
| none          | 83.60              | 2.96   |
| mild          | 83.33              | 3.90   |
| moderate      | 82.96              | 3.31   |
| heavy         | 82.65              | 2.96   |
| severe        | 83.13              | 2.72   |

----

![](/img/a005/stride-density.svg)

| Fatigue Level | Mean Stride (meters) | StdDev |
|---------------|----------------------|--------|
| none          | 1.10                 | 0.0714 |
| mild          | 1.12                 | 0.0808 |
| moderate      | 1.11                 | 0.0783 |
| heavy         | 1.09                 | 0.0759 |
| severe        | 1.07                 | 0.0767 |

----

![](/img/a005/gct-density.svg)

| Fatigue Level | Mean GCT (ms) | StdDev |
|---------------|---------------|--------|
| none          | 284           | 18.42  |
| mild          | 285           | 19.08  |
| moderate      | 286           | 16.48  |
| heavy         | 289           | 15.78  |
| severe        | 287           | 15.00  |

----

![](/img/a005/vosc-density.svg)

| Fatigue Level | Mean VOSC (mm) | StdDev |
|---------------|----------------|--------|
| none          | 67             | 5.13   |
| mild          | 69             | 5.50   |
| moderate      | 69             | 4.82   |
| heavy         | 69             | 4.55   |
| severe        | 68             | 4.98   |

## Conclusions

I'm not sure what conclusions to draw from this data, apart from the fact that
the plots look pretty.  The only running form metric that seems to change as I
get more tired is the stride, which gets shorter.


The script used to generate the plots in this blog post is available
[here](https://gist.github.com/alex-hhh/5743eb50e792aae6c9859c50f62b5517)
