    Title: Timezone Aware Local Time
    Date: 2019-10-26T11:18:30
    Thumbnail: /img/a032/thumb.png
    Tags: activitylog2, racket

I wanted [ActivityLog2][al2] to show that my New Zealand skiing run started at
10am, rather than showing 6am, which is the local time in Western Australia,
where I live.  This feature took a long time to implement, plus it required a
surprising amount of effort, and this blog post describes some of the
implementation details.

<!-- more -->

The user visible changes are very modest and almost unnoticeable: all the
timestamps display the time where an activity took place, rather than the
local time where the activity is viewed.  In the screen-shot below, the start
time for the skiing run is shown as 9:48am, even though it would have been
5:48am in Western Australia where I live.  The only "hint" is a time zone
abbreviation in the session inspector:

![](/img/a032/local-time.png)

All the other places where timestamps are shown, such as the activity list or
the lap viewer will display the time relative to the destination location and
in addition to that, the user can edit the time zone for an activity, although
for activities with GPS data this is automatically detected on import.

Implementing this "simple" feature took about two years of calendar time and
many weekends of actual work, doubled the size of the application installer
and increased the complexity of the build process.  Looking at the 1300 lines
of changes, one could wonder if the feature was worth it.  On the other hand,
I did learn a few things during this process and I no longer have to wonder
why my activities recorded in Europe or New Zealand show up at such strange
times of day.

## Time-zone Aware Time Display

[ActivityLog2][al2] stores times for activities as UTC seconds, which is are
integers representing the number of seconds since January 1st 1970.  The
Racket libraries can convert this into a `date`[racket] structure using
`seconds->date`[racket].  This conversion can be done in two ways: either show
the UTC time or the local time on the computer where the code runs:

```racket
> (define now (current-seconds))
> now
1571464832
> (seconds->date now #f)
(date* 32 0 6 19 10 2019 6 291 #f 0 0 "UTC")
> (seconds->date now #t)
(date* 32 0 14 19 10 2019 6 291 #f 28800 0 "W. Australia Standard Time")
```

In the example above, the same number, 1571464832, can be converted to show
either 6am in UTC or 2pm in Western Australia where I run the code.  There is
no built-in way to use the local time in another place, such as Auckland.  We
can however use the [tzinfo][tzinfo] package to determine the timezone offset
for another time zone, such as "Pacific/Auckland", apply the offset to the
time stamp than convert it to a date structure.  All this would show us that
it is 7pm in Auckland as I write this:

```racket
> (require tzinfo)
> (utc-seconds->tzoffset "Pacific/Auckland" now)
(tzoffset 46800 #t "NZDT")
> (seconds->date (+ now 46800) #f)
(date* 32 0 19 19 10 2019 6 291 #f 0 0 "UTC")
```

It is perhaps worth clarifying that a time zone name such as
"Pacific/Auckland" refers to a set of rules for determining the local time,
and we need to call `utc-seconds->tzoffset` for every time stamp, as the
offset itself might be different during daylight saving.

The `date`[racket] structure as obtained above can than be passed to various
date and time formating functions, which produce the strings to be displayed
to the user.

## Automatic Time Zone Detection

All that is required to display the local time is to store a time zone name
for each session and use it when formatting times.  The simplest solution is
to prompt the user to update the time zone manually for every activity they
import, and the [tzinfo][tzinfo] package does provide an `all-tzids`[racket]
function to list all time zone names and construct a GUI drop down control,
but it is impolite for the application to ask the user for information that it
can determine by itself.  Besides, I have 2524 activities in the data base,
and 1349 of them have GPS information -- I am the main user of ActivityLog2
and I was not going to manually update all of them.

The solution is to have a function which can determine the time zone name from
GPS location data, since most of the activities already have location data.
There are several packages that do this, here is [one][timezonefinder] for
Python and [one][tz-lookup] for JavaScript.  Unfortunately,
pkgs.racket-lang.org didn't have one for Racket.

I could have just copied the implementation of one of these libraries, the
[tz-lookup][tz-lookup] one in particular is relatively simple, but instead I
decided to try to understand the underlying mechanism for determining a time
zone from latitude and longitude so I came up with my own implementation.
After several failed starts, which I kept to myself, I produced the
[tzgeolookup][tzgeolookup] package along with a few blog posts describing how
it works, [here][tz1], [here][tz2] and [here][tz3].  The end result is that I
now have a function which can determine the time zone from a GPS coordinate
and this can be used to determine the time zone when an activity is imported.

```racket
> (require tzgeolookup)
> (lookup-timezone -45.06357632	168.8221158)
"Pacific/Auckland"
```

## Database schema updates

The [tzinfo][tzinfo] and [tzgeolookup][tzgeolookup] packages provide the
building blocks for formatting timestamps in local time, the next issue to
address was storing the time zone information in the database. The time zone
could be determined at runtime using `lookup-timezone`, but this can only be
done for activities which have GPS information; for other activities, such as
Lap Swimming, this information has to me manually entered by the user and
stored in the database.

The session information is stored in a [SQLite][sqlite] database, so I needed
to (1) update the database schema and (2) ensure that databases already
created using previous versions are upgraded as needed.  Since this is not the
first time I had to update the database schema, this process is fairly mature
by now: the [main database schema][dbschema] was updated to add a time zone to
the `A_SESSION` table and a [migration script][migration32] was written to
update an existing database to the new version and registered with the
application:

```sql
create table E_TIME_ZONE(
  id integer not null primary key autoincrement,
  name text unique not null);

create unique index IX0_E_TIME_ZONE on E_TIME_ZONE(name);

insert into E_TIME_ZONE(name)
values ('Africa/Abidjan'), ('Africa/Accra'), ...; -- all time zones listed here

alter table A_SESSION add time_zone_id integer references E_TIME_ZONE(id);

update SCHEMA_VERSION set version = 32;
```

Rather than storing the time zone by name in the `A_SESSION` table, a separate
table `E_TIME_ZONE` is used to store the time zone names and the `A_SESSION`
table references these using a foreign key.  This design makes selection
queries more complex, but it tries to minimize the chance that a non-existent
time zone is stored in the database: a non-existent time zone cannot be just
inserted into the `A_SESSION` table, instead the `E_TIME_ZONE` table has to be
updated first.

The [ActivityLog2][al2] application uses a somewhat unusual database upgrade
mechanism, where an initial database is always created from a full database
schema file, while previous versions are updated using migration scripts.
This creates some potential for mistakes, since a new database schema might
not be identical to an upgraded one, but there are extensive tests, run during
CI builds, which ensure that (1) new databases can be created without error,
(2) existing databases can be upgraded without error, and (3) activities can
be imported into the database.

One limitation for this [upgrade mechanism][migration] is that it is only able
to run SQL scripts, so anything that cannot be expressed in SQL, cannot be
part of a database upgrade.  For the first time for an upgrade, and this is
number 32, I wanted to update the time zone for activities already in the
database, but this is not possible using just SQL statements.

Rather than spending even more time on implementing a Racket migration
mechanism, I opted for the simpler route of providing an "Update time
zones..." tool, available in the Tools menu, which runs the update code on all
activities in the database.  The code is small and simple: an SQL query
retrieves the list of sessions which have GPS information, along with the
coordinates of their start location:

```sql
select P.session_id,
       T.position_lat as lat,
       T.position_long as lon
 from A_TRACKPOINT T, A_LENGTH L, A_LAP P
where T.length_id = L.id
  and L.lap_id = P.id
  and T.position_lat is not null
  and T.position_long is not null
group by P.session_id
```

A small racket program iterates over the result set from this query,
determines the time zone using `lookup-timezone` and updates the `A_SESSION`
table:

```racket
;; Prepare a hash to map timezone names to their id
(define tzids
  (for/hash ([data (query-rows database "select id, name from E_TIME_ZONE")])
    (match-define (vector id name) data)
    (values name id)))

(define session-data (query-rows database "***SQL QUERY HERE***"))
(for ([data (in-list session-data)])
  (match-define (vector sid lat lon) data)
  (define timezone (lookup-timezone lat lon))
  (if timezone
      (let ([tzid (hash-ref tzids timezone #f)])
        (if tzid
            (query-exec database "update A_SESSION set time_zone_id = ? where id = ?" tzid sid)
            ;; This might indicate that the time zones in E_TIME_ZONE are
            ;; outdated and need updating...
            (printf "Could not find E_TIME_ZONE.id for time zone ~a" timezone)))
      (printf "Could not find timezone for location lat ~a lon ~a" lat lon)))
```

The application already had a mechanism of running a batch task against the
database, together with a progress bar and a mechanism to cancel the jobs, so
it was easy to add this upgrade code to the application GUI:

![](/img/a032/update-time-zones.png)

## Allowing the User to Edit the Time Zone

Automatic time zone detection is convenient, but it is not always possible, as
not all activities have GPS information.  The two major types of such
activities (that I have anyway) are Lap Swimming sessions and bike sessions
done on an internal trainer.  This means that the application must allow the
user to edit the time zone for an activity.  There are two places where this
can be done: the first one is the session inspector headline which allows
switching to "Edit" mode, to allow the user to quickly adjust the title, sport
and now time zone:

![](/img/a032/edit-headline.png)

The second place is the dialog where the full activity details can be edited.
As a side note, the activity editor was one of the first things I wrote for
ActivityLog2, doing it while still learning Racket, and the code base was
"less than ideal", I would probably approach the task differently if I had to
do it again, but my opportunities to do things a second time are limited...

![](/img/a032/activity-edit.png)

## Package Dependency Management

Perhaps it would be a surprise to many, but ActivityLog2 used only packages
that are part of the main Racket distribution, with no external packages
needed.  I always liked the fact that one could just install Racket, clone the
ActivityLog2 repository and run the application's `run.rkt` file.

This has changed now: the [tzinfo][tzinfo] and [tzgeolookup][tzgeolookup]
packages, plus their dependencies, must be installed for the application to
build and run.  These dependencies have created a few problems related to
package management.  For a simple use case, one can install the dependent
packages from the default Racket package catalog using a single `raco`
command:

```
raco pkg install --auto tzinfo tzgeolookup
```

Alternatively, the dependencies can be added to an `info.rkt` file and
converting ActivityLog2 into a pseudo-packages, so the dependencies are
installed when the user runs `rack pkg install` in the application directory.

However, the problem with this approach is that these package dependencies are
not versioned: `raco pkg install` will always install the latest package
available in the catalog server, and this is not what is intended if an older
version of the application is checked out.  Also, and more importantly for my
use case, some of the packages might contain bugs which are not yet fixed.  An
example is the `tzinfo` package which has a bug preventing its installation on
Windows.  While I submitted a pull request for [this issue][tzinfo-10], I
wanted to be able to use the fixed version until this issue is fixed and an
update is available on the official package server.

The solution was to add all dependent packages as git submodules to the source
repository of the application, and let git manage the versions of these
packages.  During the build, a new package catalog is created using the
`pkg/dirs-catalog` command, and this is added to the list of package catalogs:

```sh
# Go to the direcory containing the packages
cd pkgs
# Index the directory as a package catalog
racket -l- pkg/dirs-catalog --link catalog .
# Add the location for this catalog to catalog-locations.txt
echo "file://`pwd`/catalog" > ./catalog-locations.txt
# Also add existing catalogs to the same file
raco pkg config catalogs >> ./catalogs-locations.txt
# Set the new list of catalog locations (note that the pkgs catalog is first)
raco pkg config --set catalogs `cat ./catalog-locations.txt`
```

The actual **script** run by the build is a bit more complex, but once the
catalog is set up, the packages can be installed using:

```
raco pkg install --auto tzinfo tzgeolookup
```

Why not just link the packages in place?  After all, the build script could
have just ran a `raco pkg install` in each package folder.  The "catalog"
mechanism solves additional problems:

* the package dependencies are also resolved from this catalog and don't have
  to be manually installed in the correct order.  The build script needs to
  install only the direct dependencies of ActivityLog2
  
* on CI build pipeline, the catalog setup will *replace* the existing catalogs
  with this directory based catalog.  This ensures that any package
  dependencies can only be resolved through this catalog and a package install
  will fail if a dependent package is not version controlled -- this is a
  simple mechanism to ensure that all package dependencies are under version
  control.
  
* the source location for the packages is disconnected from the installation
  process: if the catalog setup is skipped, packages will be installed from
  the usual Racket catalog, which might come in handy.

This mechanism offers flexibility but also has some limitations, as the
package installation is global, meaning that any program which wants to use
the `tzinfo` or `tzgeolookup` packages will use the ones installed from the
ActivityLog2 "pkgs" folder.  It would be great if the packages could be
installed for a specific application only, but I could not find a way to do
that.

## Final Thoughts

This was a large task to implement: it took me two years since I recorded this
as an [issue][issue-11], the commit is [over 1500 lines of code][commit], the
resulting binary application size increased by about 25Mb, and I quite
possibly introduced some bugs along the way.  As I look back, I question if it
was worth it.  Well, as far as usefulness goes, the answer is probably no,
since I only have a few activities which were recorded out of my regular time
zone, I could have lived without this feature.  On the other hand, I did learn
a wide variety of things, from working with git submodules, setting up custom
package catalogs, and implementing efficient lookup algorithms for geographic
location lookup, and I think this aspect made the task worth it.

[al2]: https://github.com/alex-hhh/ActivityLog2
[tzinfo]: https://pkgs.racket-lang.org/package/tzinfo
[timezonefinder]: https://github.com/MrMinimal64/timezonefinder
[tz-lookup]: https://github.com/darkskyapp/tz-lookup
[tzgeolookup]: https://pkgs.racket-lang.org/package/tzgeolookup
[tz1]: /2019/08/timezone-lookup.html
[tz2]: /2019/08/timezone-lookup-2.html
[tz3]: /2019/05/timezone-visualization.html
[sqlite]: https://sqlite.org/index.html
[tzinfo-10]: https://github.com/97jaz/tzinfo/pull/10
[al2-11]: https://github.com/alex-hhh/ActivityLog2/issues/11
[dbschema]: https://github.com/alex-hhh/ActivityLog2/blob/master/sql/db-schema.sql
[migration32]: https://github.com/alex-hhh/ActivityLog2/blob/master/sql/migrations/p32-time-zone.sql
[migration]: https://github.com/alex-hhh/ActivityLog2/blob/250bb47d2959a07abd74b475f6b240c69e4fd8cb/rkt/dbapp.rkt#L38
[issue-11]: https://github.com/alex-hhh/ActivityLog2/issues/11
[commit]: https://github.com/alex-hhh/ActivityLog2/commit/250bb47d2959a07abd74b475f6b240c69e4fd8cb
