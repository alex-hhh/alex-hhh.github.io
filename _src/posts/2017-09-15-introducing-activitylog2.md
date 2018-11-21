    Title: Introducing ActivityLog2
    Date: 2017-09-15T20:26:56
    Tags: activitylog2
    Thumbnail: /img/a001/thumb.png

[ActivityLog2](https://github.com/alex-hhh/ActivityLog2) is a program that can
analyze data recorded by fitness devices such as swim, bike and run sessions.
It can be used as a training log and to gain insights about your training
using reports and trend charts.

<!-- more -->

I have been using ActivityLog2 exclusively for about three years and I also
imported my previous data (since 2011) into the database.  As of today, my
training database contains 2011 training sessions and 2469685 data points, it
is 462 Mb in size.

You can find the application on GitHub at
[https://github.com/alex-hhh/ActivityLog2](https://github.com/alex-hhh/ActivityLog2)
and you can download a pre-built release from the "Releases" tab.

<p align="center">
<img align="center" width="800" 
     alt="session view" 
     src="https://drive.google.com/uc?export=download&id=0B5h4XOdkim72VmxOOWhYeVN5TGs" />
</p>

## So why did I write it?

I purchased my first GPS device, a Garmin Forerunner 310XT back in June 2011
and started using the Garmin Connect web site to analyse activities and track
my progress.  While not perfect, the site was sufficient for my needs and I
was happy with it.  In 2013 I started training for my first Ironman race, and,
about two months before the race, Garmin started upgrading their Garmin
Connect web site to a newer version.  Unfortunately, this process did not go
smoothly and the site was down for extended periods of time.  I was sitting in
front of my computer, with the Forerunner in my hand and I could not view my
latest ride or run because some computer in a data centre half way across the
globe was not working properly.

It was around that time that I decided to write my own tool to analyze
training data, as I did not want to depend on other people's servers.  I
wanted an platform that I can fully control, both the training data itself and
the software that analyses the data.  Here is how ActivityLog2 meets this
criteria:

* Data is stored on the local computer and you always have access to it as
  long as you can access your computer.  You are responsible for backing up
  your data, of course.
  
* Data is stored in an SQLite database, and can be easily accessed from most
  programming languages, without need to go through the ActivityLog2
  application.  In particular, I have several Excel spreadsheets that connect
  directly to it for some of the reports.

* The original activity files are stored in the database unmodified, and are
  always available if you want to export them to use on some other platform.

* The ActivityLog2 application is freely available in source code, and anyone
  can see how it works and update it to meet their needs without requiring my
  permission

* The ActivityLog2 application already has a decent set of features, but in
  addition to this, all data processing algorithms are available as
  programming interfaces. This makes it a good starting platform for writing
  your own analysis scripts. For example, I have scripts that generate
  workouts to download on the Garmin devices, and scripts that perform FTHR
  analysis.

Sometimes after I started working on ActivityLog2, became aware of Golden
Cheetah, a program that would have met the criteria, but at that time, GC only
supported bike ride activities.  This has changed now, but by this time
ActivityLog2 has matured enough that I'm happy to use it exclusively to track
my activities.

