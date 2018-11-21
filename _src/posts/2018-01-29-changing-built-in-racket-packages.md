    Title: Changing Built-in Racket Packages
    Date: 2018-01-29T19:12:09
    Tags: racket
    Thumbnail: /img/a009/thumb.png

To modify one of the Racket built in packages, a local copy of the package has
to be set up first.  This is a surprisingly complicated task and I spent more
time on it than I would like to admit.  To save future me and others some
time, I wrote up a set of instructions on how to do it.

<!-- more -->

In my case, I wanted to add some features to the
[plot](https://docs.racket-lang.org/plot/index.html) package which ships by
default with a Racket installation.  I started by following the instructions
in this
[post](https://groups.google.com/forum/#!starred/racket-users/eP6QLUD8DlA) on
the Racket Users mailing list, but it was missing some information, which must
have seemed obvious to experienced Racket developers.  The steps below contain
a hopefully complete list of steps.  They work on a Windows 10 machine, with
the user having no administrator privileges.

## Switch to the development version of the plot package

The plot repository actually contains six packages: "plot", "plot-compat",
"plot-doc", "plot-lib", "plot-gui-lib", and "plot-test".  They are installed
as part of the normal Racket installation, which racket calls "installation
scope".  To modify that installation, administrator privileges are needed.
Alternatively, the packages can be installed in the "user scope", as shown in
the commands below.  Running these commands below will some time as packages
and dependencies are rebuilt:

    raco pkg install --force --scope user --catalog https://pkgs.racket-lang.org plot
    raco pkg install --force --scope user --catalog https://pkgs.racket-lang.org plot-lib
    raco pkg install --force --scope user --catalog https://pkgs.racket-lang.org plot-gui-lib
    raco pkg install --force --scope user --catalog https://pkgs.racket-lang.org plot-doc

## Clone the plot repository from GitHub and link the packages

Next step is to clone the plot package using the command below:

    git clone https://github.com/racket/plot.git
    
This will give access to the source code and we can now tell racket to use the
source code inside the repository for the packages.  This is done using a
"raco pkg update" command.  Note that there is a trailing slash at the end of
the package names: without it, the package name will be looked up in the
catalog instead of the specified directory:

    cd plot
    raco pkg update plot-lib/
    raco pkg update plot-gui-lib/

Finally, we can check that the plot packages are indeed installed in a git
clone and can be modified:

    $ raco pkg show --all --long --rx 'plot-.*'
    Installation-wide:
     Package[*=auto]    Checksum                                    Source
     plot-compat*       cfae0fb19a8c83d6f756cea9d9815f50bb170e05    (catalog "plot-compat")
     plot-doc*          74c466400fb4ece74cef869f3d02d81fb176ff7c    (catalog "plot-doc")
     plot-gui-lib*      7ef4b214d028877e9d0003e357d77bd7e94db967    (catalog "plot-gui-lib")
     plot-lib*          8408c8d63a94ee2cd57713c40fd7e68525107014    (catalog "plot-lib")
    User-specific for installation "6.11":
     Package         Checksum    Source
     plot-gui-lib    #f          (link "C:\\Users\\Alex\\rkt-libs\\plot\\plot-gui-lib")
     plot-lib        #f          (link "C:\\Users\\Alex\\rkt-libs\\plot\\plot-lib")

The "plot-gui-lib" and "plot-lib" packages are listed twice.  This is because
the second installation is in "user scope".  These packages are also listed as
being linked to a directory where we cloned the plot package.  Since the user
scope is searched first, racket will load these packages from the git
repository.  The source for these packages can now be modified and racket will
pick up the changes automatically.

## Switching git remotes

In the step above, the main plot package was cloned.  Unless you have push
access to that repository, it is better to switch to your own fork.  Assuming
the plot package is already forked on GitHub, the origin branch can be
renamed to upstream:

    git remote rename origin upstream
    
... and the fork added as the "origin" remote:

    git add remote origin git@github.com:alex-hhh/plot.git
    
