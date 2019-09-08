    Title: Running and Cycling Workout Editor
    Date: 2018-05-27T08:37:52
    Thumbnail: /img/a014/thumb.png
    Tags: activitylog2

A Garmin fitness device can use workouts during a bike or run activity: they
are a collection of steps with a duration and intensity (such as heart rate,
pace or power).  While running or biking, the device keeps track of the
duration and will sound an alarm if the intensity is too high or too low.
Workouts can be created on the Garmin Connect website, but I decided to write
my own application for creating workouts.

<!-- more -->

Garmin devices use the "Fitness Interchange Format" (files with the "FIT"
extension) for any data that is transferred to and from the device.  The data
received from the device is most often activity or session data, so FIT files
are more commonly associated with fitness activities.  However, FIT files are
used for other things as well: in particular, device settings and workouts can
be sent to the device as a FIT file.  "Sending to the device" in this case
means simply copying the file: when the Garmin device is connected to the
computer via USB, it shows up as a normal USB Drive and files can be copied on
the "NewFiles" folder on that device.

<div style="text-align:center">
<iframe width="700" height="396" src="https://www.youtube.com/embed/8kA0-ahHxGI" frameborder="0" gesture="media" allow="encrypted-media" allowfullscreen></iframe>
</div>

## A programmers workout editor

Relatively early on in the ActivityLog2 project, I added support for writing
out FIT files, especially workouts and settings.  Since I am a programmer, I
just wrote a small library which allowed to write workouts something like
this:

```racket
(define (hr/warmup) (hr/bpm 60 163))
(define (pace/hard) (speed (pace 4 29) (pace 4 0)))
(define (pace/easy) (speed (pace 7 32) (pace 5 7)))
(define (pace/tempo) (speed (pace 4 45) (pace 4 31)))

(define (wk-daniel-q10)
  (define wk (new fit-workout-file% [name "daniel-q9"] [sport 1]))
  ((workout
    (step (miles 2) (hr/warmup) warmup)
    (repeat
     4
     (step (miles 1) (pace/tempo) active)
     (step (minutes 1) (no-target) recover))
    (step (minutes 5) (pace/easy) recover)
    (repeat
     3
     (step (miles 1) (pace/tempo) active)
     (step (minutes 1) (no-target) recover))
    (step (miles 2) (pace/easy) cooldown)
    (step (lap-button) (pace/easy) cooldown))
   wk)
  (send wk get-fit-data))
```

To someone familiar with writing (or reading) code it should not be too
difficult to determine that the code above defines the following workout:

* A warmup step, 2 miles long with a "hr/warmup" heart rate (this is defined
  above as being a Heart Rate between 60 and 163 BPM
* A repeat (four times) of: 1 mile at tempo pace (this is defined above as
  4:45 to 4:31 min/km followed by 1 minute recovery
* 5 minutes at an easy pace (defined above as 7:32 to 5:07 min/km)
* another repeat (three times) of: 1 mile at tempo pace followed by 1 minute
  recovery
* 5 minutes cooldown at an easy pace
* another cooldown step which lasts until the "Lap" button is pressed on the
  watch

As a side note, the code above defines a `wk-daniel-q10` function, which, when
called, returns a byte string representing the binary FIT workout data.  This
data can be written to a file and transferred to the device.

This method of writing workouts has the advantage that workouts can be written
using text editors, and they are really Racket programs, so one has the full
flexibility of a programming language when defining them.  The library that is
used to write these workouts is about 150 lines of code and I was able to
write it in an afternoon. 

## A GUI workout editor

I have used the above method for writing workouts for the past four years to
write workouts for my training needs, but it is not for everyone.  After a
while I realized that it has some limitations: even as a programmer, when it
comes to managing my training, I prefer a GUI approach, with immediate
feedback.  In particular, I wanted feedback on the estimated duration of the
workout, as well as percentage of time spent in "active" vs " recovery".
Writing a GUI application is more difficult than a simple library so I kept
postponing this project (for comparison, the GUI workout editor is about 3000
lines of code vs about just 150 for the helper library).  However, I had some
more spare time than usual in the last few weeks, so I decided to try to
implement it.  The result is here:

![](/img/a014/workout-editor.png)

The workout editor is not released yet (there are still some bugs that need
fixing), so you will need to download the [source
code](https://github.com/alex-hhh/ActivityLog2) and build it.

What it can do now:

* build bike and running workouts based on Heart Rate, Pace or Power
* estimate the duration of a workout, useful if you want your workout to fit
  in a specified time (e.g. 1 hour), as well as percentage of time spent in
  warmup/cooldown or active/rest.
* generate FIT workout files which can be used on a Garmin device (other
  devices might work too.
  
Plans for the future:

* Since workouts are stored in the database, activities can be linked back to
  workouts.  This would be useful in providing better estimates for workout
  duration/distance/effort.
* Generate ERG/MRG workout files for use on a [bike
  trainer](/2017/11/bike-trainer.html)
* In addition to time estimates, provide plots of estimated W'Bal expenditure
  and recovery -- a Critical Power model can determine if the designed workout
  is too hard or too easy, which would be really cool.

