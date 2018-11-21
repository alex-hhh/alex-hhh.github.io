    Title: Running and Outdoor Temperature
    Date: 2017-12-21T17:09:45
    Tags: training data analysis
    Thumbnail: /img/a007/thumb.png
    
It is no surprise to anyone who tried to run in the heat that it is harder
than when the outdoor temperature is more pleasant.  But how much harder is
it, and how can this be quantified?  With summer in full swing in the southern
hemisphere, I decided to take a look at how running is affected by
temperature.

<!-- more -->

Between 2011 and today, I have recorded 741 running sessions in my
[ActivityLog2][al-link] database.  Out of these sessions, some were run
without my Garmin watch and for some of them either the GPS or the heart rate
monitor has misbehaved, recording invalid data.  This has left me with 700
sessions for which I have accurate speed, heart rate and weather data.

## Temperature

In Western Australia we have plenty of hot days, especially during
summer. Whenever possible, I try to run when the temperature outside is
reasonable, but this is not always possible.  Below is the distribution of my
running sessions with regards to outdoor temperature, as you can see I prefer
to run when the temperature outside is between about 18 and 23 ℃.

![](/img/a007/temperature-hist.svg)

Temperature does not tell the whole story about how it feels outside: humidity
and barometric pressure also have an influence on the comfort level.  Many
weather applications can show a "feels like" temperature which takes into
account humidity and barometric pressure.  There are several formulas for
calculating this "feels like" temperature.  I experimented with several of
them and I decided to use [Humidex][hx-link], mainly because it can be simply
calculated and it matched my own perceived feel for the temperature.  I have
been using Humidex for a while now, and every time I record a run, I check
this value to see if it seems OK.  So far, I have been happy with this
formula.  Below is a distribution of my running sessions with regards to
Humidex.  Compared to the temperature distribution, the entire plot is shifted
to the right, indicating that the "feels like" temperature for my runs is a
lot warmer that temperature would indicate.  It seems that the "feels like"
temperature when I prefer to run is somewhere between 21 and 26 ℃.

![](/img/a007/humidex-hist.svg)

## Efficiency Factor

We need a metric to describe the efficiency of running and compare it with
temperature.  A good metric seems to be [efficiency factor][ef-link] which is
simply the ratio of the *running speed* and *heart rate*:

    EF = Running_Speed / Heart_Rate
    
This ratio ties together the **effort** required by the run, as measured by
the **heart rate** and the **power output** of the run, as measured by the
**running speed**.  An individual EF value has no meaning, and EF values
cannot be compared between two athletes, but tracking the EF for an individual
athlete can show if they are improving or not: maintaining the same running
speed at a lower heart rate means that the athlete has improved and will
result in a higher EF.  An increased EF can also result from being able to
maintain a higher running speed at the same heart rate.

The efficiency factor is a great metric, since it is independent of the type
of run: a high intensity run will have a higher average heart rate than a slow
easy run, but it will also have a higher speed, so the overall EF of these two
running sessions should be about the same.  It will never be probably
identical, since heart rate is affected by other factors such as outdoor
temperature and how fresh or tired the athlete is.

Another factor that can influence EF is how hilly the running course is: when
running uphill, the running speed will decrease while heart rate will increase
(to put it differently, it is harder to run uphill than it is on the flat).
In such a case, EF will be lower than if running on a flat course.  To
compensate for this, the notion of **grade adjusted pace** (GAP) is
introduced: this takes into account the running speed and the current slope
and produces an equivalent speed as if the run happened on flat ground.  When
running uphill, GAP will be higher than the actual pace, while running
downhill it will be lower.  As with "feels like' temperature, there are
several algorithms for GAP, the one I implemented is the one from Strava,
which you can find described [here][gap-link].  When calculating EF, the speed
is first adjusted for grade, this ensures that the calculated EF values take
into account how hilly the running course was.

## Comparing efficiency factor with temperature

I calculated the efficiency factor for all my running sessions and plotted
them against the "feels like" temperature for the run.  This is the gray dot
could in the plot below.  There are several EF values for each temperature,
except the extremes where there are only a few data points.  This is because
EF actually tracks fitness level and over the 6 years of data my fitness level
varied quite a bit, being low at the start of a season and high at the end of
the season.  To smooth things out, I calculated the average EF for all
sessions at a certain temperature, these re the big orange dots on the plot.
It is now quite apparent that there is a correlation between the efficiency
factor and temperature.  I fitted a second degree polynomial over these data
points to obtain a smooth line (the blue dashed line) on the plot.

![](/img/a007/humidex-ga-ef-scatter.svg)

The fitted polynomial allows estimating the efficiency factor given a
temperature value as follows:

    EF = 1.8347990118 + 0.0114733678 * humidex - 0.0002975829 * (humidex ^ 2)

The above formula indicates that the maximum EF of 1.95 is at 19.28 ℃, so
this is the optimal temperature for me to run -- intuitively, this is about
right.  We can do more with this formula, for example, we can determine how
much HR increases with temperature if the same running speed is maintained (EF
ratio represents the percentage of EF drop when compare to the best EF at
19.28 ℃):

|          | 19.28 ℃ | 20 ℃ | 25 ℃  | 30 ℃  | 35 ℃  | 40 ℃  |
|----------|---------|------|-------|-------|-------|-------|
| EF       | 1.95    | 1.95 | 1.94  | 1.91  | 1.87  | 1.82  |
| EF ratio | 100%    | 100% | 99.5% | 98.2% | 96.2% | 93.4% |
| HR       | 150     | 150  | 151   | 153   | 156   | 161   |
| HR       | 160     | 160  | 161   | 163   | 166   | 171   |
| HR       | 170     | 170  | 171   | 173   | 177   | 182   |

Alternatively, we can determine how much running speed will have to decrease
to keep a constant heart rate when temperature increases:

|               | 19.28 ℃ | 20 ℃ | 25 ℃  | 30 ℃  | 35 ℃  | 40 ℃  |
|---------------|---------|------|-------|-------|-------|-------|
| EF            | 1.95    | 1.95 | 1.94  | 1.91  | 1.87  | 1.82  |
| EF ratio      | 100%    | 100% | 99.5% | 98.2% | 96.2% | 93.4% |
| Pace (min/km) | 6:00    | 6:00 | 6:02  | 6:06  | 6:14  | 6:25  |
| Pace (min/km) | 5:00    | 5:00 | 5:02  | 5:05  | 5:12  | 5:21  |
| Pace (min/km) | 4:30    | 4:30 | 4:31  | 4:35  | 4:41  | 4:49  |
| Pace (min/km) | 4:00    | 4:00 | 4:01  | 4:04  | 4:09  | 4:17  |

So how useful is this?  The above data is from a single athlete (me), and I am
acclimatized to heat, as I live in a hot climate.  The above values match my
empirical experience on how to adjust my running as the heat increases.

How about **running in the cold**? The EF values seem to decrease as
temperature drops.  I have much less data for running in the cold and you can
also see that the average EF data points don't follow the fitted line as
closely as they do for higher temperatures.  I have no direct use for this
data, so for now I didn't calculate any tables for cold temperatures.

## Technicalities

The data was extracted from ab [ActivityLog2][al-link] database, containing
all my training history data, using a [Racket][racket-link] script, while the
polynomial model fitting was done in [R][r-link] and [R Studio][rstudio-link].
The plots were generated using the Racket built-in plot package.

The scripts used to generate the data can be found in this [GitHub
Gist][gist-link].

I tried to fit several models, but the best fit was the EF with GAP compared
to humidex.  If you want to check fit the parameters, you can find them
[here][fit-summary-link].

[al-link]: https://github.com/alex-hhh/ActivityLog2
[hx-link]: https://en.wikipedia.org/wiki/Humidex
[ef-link]: https://www.trainingpeaks.com/blog/efficiency-factor-and-decoupling/
[gap-link]: https://medium.com/strava-engineering/an-improved-gap-model-8b07ae8886c3
[gist-link]: https://gist.github.com/alex-hhh/41b94005339724421b9832c74fe54d92
[racket-link]: http://www.racket-lang.org/
[r-link]: https://www.r-project.org/
[rstudio-link]: https://www.rstudio.com/
[fit-summary-link]: https://gist.github.com/alex-hhh/41b94005339724421b9832c74fe54d92#file-fit-summary-txt
