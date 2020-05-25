    Title: Dependency Management in Racket Applications
    Date: 2020-05-23T07:40:59
    Thumbnail: /img/a038/thumb.png
    Tags: racket

.. in which we look at how to manage the versions of packages used by a Racket
application, and ensure that the correct package versions are used when
building the application.

<!-- more -->

A package can be installed using the "raco pkg install" command, and this will
consult a "package catalog" (by default "pkgs.racket-lagng.org"), to determine
the download location for the package.  "raco pkg install" will always install
the latest package version available in the catalog and there are no ways to
specify that an application depends on a specific version of the package.

In an ideal world, this is not a problem: packages should be well tested,
contain no bugs and their API always backwards compatible.  This means that
each time you run "raco pkg install" you'll get a package that was at least a
good as last time you run the command, or possibly a better version.  However,
things are never that simple.

## What problem are we trying to solve?

In one of the first packages that I started using in [ActivityLog2][al2], I
discovered a bug, which made the package unusable for me.  I was even able to
provide a fix for this bug, but the package author was busy, and about two
months passed before the fix was actually merged in the official package.
Even though I had a fix for the bug, my only option was to wait until an
updated package was released.  Unfortunately, this was not the only incident,
and I found defects in other packages as well.

From my own experience, I realized that I could not rely on the package
catalog to ensure that I always have working packages to depend on.  I also
understand that package authors are busy, and what might be an important bug
for me might not be for them.  However, if my Racket application was to use
other peoples packages, I had to be able to better manage these dependencies.
In particular, I wanted to be able to do the following:

* If I find a bug in a package, and have a fix for it, I should be able to
  setup an alternate install location for the package, containing the bug fix,
  and keep using that location until the author releases an updated version.

* If I find a bug, but don't have a fix, I should be able to install a
  previous version of the package, without the bug, and keep using that
  version until an updated version is released.

* I want to ensure that the continuous integration build uses the same package
  versions that were used during development.  Running a build on an older
  branch will use the package versions which were used on that branch.

* Finally, I want to be able to upgrade packages at my own pace: every time
  packages are upgraded, the application needs to be tested and any
  incompatibilities fixed.  I wanted to avoid the situation where I have to
  stop working on a feature, because the build on the continuous integration
  server fails when a later version of a package was installed.
  
## Version Management using GIT Sub-Modules

Since I was already using GIT, I decided to start using [git
sub-modules][git-sm] to track the versions of the packages.  Each package that
my application depends on is a GIT sub-module in a "pkgs" sub-folder, and will
be installed from this folder instead of the Racket package catalog.

![](/img/a038/sub-modules.png)

Git can be used to track versions at the commit level, and a submodule will
only be updated when explicitly asked to. This has several advantages:

* sub-module versions are tied to the git commit for the main application:
  when I push a commit to the Continuous Integration sever, I know the exact
  package versions it will be using for a build.  When I checkout an older
  version of the application, I know that it will have corresponding
  sub-module versions which were working with my application at that time.

* sub-modules are only updated with explicit git commands, so, as a developer
  I am in control when and how I update sub-modules to newer versions.

* sub-modules have a remote repository from which the versions are retrieved.
  By default, this is the same repository which is used for the official
  version of the package, however, if I fix a bug in my own fork of a package,
  I can point the sub-module to use my own fork until the main package is
  fixed.

Sub-modules, introduce some complexity into the application development
work-flow, for example, submodules need to be initialized after cloning the
application and need to be updated explicitly after switching to an older
commit or pulling in some changes.  However, at least in my case, I found that
the benefits of using sub-modules outweigh the additional complexity they
introduce.

## Package Installation

Packages can be installed form a local folder, so the "packages-as-submodules"
can be installed directly from their folder inside the application repository.
For example, if the source for the `data-frame` package is checked out in a
folder named "data-frame", it can be installed using:

```
raco pkg install ./data-frame
```

The above command will install a package in-place, meaning that I can update
the source files inside the package directly, and they will be automatically
used by the application.

Installing packages form a local folder has some limitations, however: most
packages depend on other packages, and these dependencies will be installed
from the normal racket catalog by default.  Once we start tracking versions,
it makes sense to track these dependent packages as well, since these too can
have bugs.

To address this problem, the dependencies of a package can also be added as
git submodules and installed from a local folder.  This means, however, that
we need to keep track of package dependencies ourselves, and this can be a lot
of work.  Package dependencies are already recorded in the "info.rkt" file for
each package, so commands like "raco pkg" can find and install them. This
information can be used with the packages-as-submodules method if we set up a
package catalog.

### Package Catalogs Overview

When the user wants to install a package using "raco pkg install", the
following locations will be consulted to find the package, each such location
is called a "package catalog":

```sh
$> raco pkg config catalogs
https://download.racket-lang.org/releases/7.7/catalog/
https://pkgs.racket-lang.org
https://planet-compats.racket-lang.org
```

First in that list is the catalog for the current Racket release, and it
contains all the packages that ship with a full Racket distribution.  Next in
the list is the "package server", `pkgs.racket-lang.org`, where everyone can
submit and register their own packages.

New catalogs can be added to this list and existing ones removed -- this
allows controlling where "raco pkg install" looks for packages.

### Directory as a Catalog

We can set up the "pkgs" directory, which contains the packages-as-submodules
folder, as a catalog by using the [dirs-catalog][dirs-catalog-module] module.
This module can be run directly from the command line and allows indexing the
packages in a directory to create a catalog:

```sh
$> cd pkgs
$> racket -l- pkg/dirs-catalog --link catalog .
```

When the previous command is run in a directory, it will scan the packages in
the directory and create an index in the "catalog" sub-folder.  This catalog
can be referred to using the "file://" URL naming scheme.  In my case this
would be:

```sh
$> echo file://`pwd`/catalog
file://C:/Users/alexh/Projects/ActivityLog2/pkgs/catalog
```

### Package Catalog Setup

With the local package catalog set up, we can now append its location to the
list of package catalogs in the first position:

```sh
# Add our catalog first
$> echo file://`pwd`/catalog > catalog-locations.txt
# Append the existing catalogs
$> raco pkg config catalogs >> catalog-locations.txt
# Set the new catalog locations
$> raco pkg config --set catalogs `cat catalog-locations.txt`
# Let's see the updated list
$> raco pkg config catalogs
file://C:/Users/alexh/Projects/ActivityLog2/pkgs/catalog
https://download.racket-lang.org/releases/7.7/catalog/
https://pkgs.racket-lang.org
https://planet-compats.racket-lang.org
```

The directory containing our packages-as-submodules is configured to be the
first one for looking up packages, so any "raco pkg install" command will now
search for packages (including packages installed as dependencies) into our
package directory first.


## Some Other Details

The previous section showed how to setup the package catalog using individual
commands, but [ActivityLog2][al2] contains a shell script,
[setup-catalog.sh][setup-catalog], which completes all these steps in one go,
so package indexing and catalog setup is done just by running this script.

There are a few extra bits, which makes this mechanism of managing
dependencies more convenient.

### Isolation Mode

When dependencies are resolved, all package catalogs are consulted starting
from the first one.  If one of the tracked packages adds a new dependency, the
dependency will not be found in our catalog, but it will be found and
installed using the standard Racket package catalog, introducing an untracked
dependency.

New dependencies can be hard to notice, especially when packages are installed
using the "--auto" flag which installs dependencies automatically, and there
is a risk that the application will start to depend silently on untracked
packages.

To avoid this situation, the Continuous Integration build sets up the
sub-modules directory as the **only** catalog available for package
installation.  This means that the CI server will fail to install a package
whose dependency is not present as a git submodule, ensuring that we catch
this situation and correct it.

### Installing the dependencies on one go

An application does not have an "info.rkt" file, and as such it cannot list
dependencies (an application can however be installed as a package).  To
simplify dependency tracking, I created a simple "dummy" package which lists
all dependencies in its info file.  The package contains one single info.rkt
file:

```racket
#lang info
(define collection "al2-dependencies")
(define deps '("tzinfo"
               "tzgeolookup"
               "data-frame"
               "plot-container"
               "gui-widget-mixins"))
(define build-deps '())
(define scribblings '())
(define pkg-desc "Meta package to install all ActivityLog2 dependencies")
(define version "0.0")
(define pkg-authors '(aharsanyi))
```

This "al2-dependencies" package is present in the "pkgs" folder, but not as a
sub-module, and it is also not published on the racket package catalog, since
it provides no useful features except for recording dependencies of the
application itself.  However, it is indexed and can be installed, and in turn
it will install all dependencies using the command:

```sh
raco pkg install --batch --auto al2-dependencies
```

### Reporting missing packages

For a long time, [ActivityLog2][al2] did not depend on any other packages
except ones that come with the normal Racket distribution.  The application
could simply be run by opening the "run.rkt" file and running the application.
Now that the application depends on other packages, it will report missing
modules if packages are not installed, but unfortunately, the error messages
are not always clear.

To improve the situation, I wrote a `check-missing-modules` macro to verify
that required modules are installed and report a more "to the point" message
to the user:

```racket
(begin-for-syntax

  ;; Return #t if the module identified by SYM can be loaded using a require
  ;; statement, #f otherwise
  (define (check-module sym)
    (with-handlers
      (((lambda (e) #t) (lambda (e) #f)))
      (and ((current-module-name-resolver) sym #f #f #f) #t)))

  ;; Check for any modules in MODULES which are missing and report them using
  ;; the error function
  (define (check-missing modules)
    (define missing (for/list ([m modules] #:unless (check-module m)) m))

    (unless (null? missing)
      (error (format "You must install these packages: ~a
*** HINT: see docs/README.md for more details" missing)))))

(define-syntax (check-missing-modules stx)
  (syntax-case stx ()
    [(_ mod ...)
     (let ([modsyms (syntax->datum #'(mod ...))])
       #`(quote #,(check-missing modsyms)))]))
```

The start files "run.rkt" and "build.rkt" contain the following line, listing
the required packages:

```racket
(check-missing-modules tzinfo tzgeolookup data-frame plot-container gui-widget-mixins)
```

And, if the user forgets, for example, to install the "data-frame" package,
and tries to run the application, it will get the following error message:

```sh
$> racket run.rkt 
You must install these packages: (data-frame)
*** HINT: see docs/README.md for more details
```

Instead of the more obscure:

```sh
racket run.rkt 
standard-module-name-resolver: collection not found
  for module path: data-frame/private/bsearch
  collection: "data-frame/private"
  in collection directories:
   C:\Users\alexh\AppData\Roaming\Racket\7.7\collects
   C:\Program Files\Racket\collects
   ... [173 additional linked and package directories]
```

### Limitations

Packages installed in Racket are global and available to every racket program
-- the user level scope is really installing packages for the current user,
but they are still available to all Racket programs for that user.  This means
that the packages installed from the packages-as-submodules folder will be
available outside the [ActivityLog2][al2] application and that no other
package versions can be installed for other applications.  Working with two
applications using the method presented here would require installing and
un-installing packages.

## Final Thoughts

I have been using this method for about 6 months now, and it has worked
reasonably well.  The method was mentioned in another [blog post][local-time],
which was when I started using it, but this blog post expands a bit both on
the reasoning behind the method as well as the actual details.

[al2]: https://github.com/alex-hhh/ActivityLog2
[git-sm]: https://git-scm.com/book/en/v2/Git-Tools-Submodules
[dirs-catalog-module]: https://docs.racket-lang.org/pkg/dirs-catalog.html
[setup-catalog]: https://github.com/alex-hhh/ActivityLog2/blob/master/etc/scripts/setup-catalog.sh
[local-time]: /2019/10/local-time.html
