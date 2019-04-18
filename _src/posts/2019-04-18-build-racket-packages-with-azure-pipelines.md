    Title: Build Racket Packages with Azure Pipelines
    Date: 2019-04-18T16:09:34
    Thumbnail: /img/a025/thumb.png
    Tags: racket
    
With the future of Travis uncertain, I started looking for a [Continuous
Integration][ci] alternative and since Microsoft advertised Azure DevOps
everywhere I went on the web, I decided to give it a try.  I now moved all my
Racket packages to Azure Pipelines, plus have a Racket application built with
it, so I decided to write up some notes about my experience.

<!-- more -->

This blog post covers setting up an Azure Pipelines [CI][ci] for a Racket
package, but it does make any attempt to make the case that it is better or
worse than the alternatives: I was looking for a Travis alternative, tried out
Azure Pipelines, found it sufficient for my needs and stayed with it -- at
least for now.

![](/img/a025/AzurePipelines.png)

Azure Pipelines does not support building Racket packages out of the box, but
as was the case with [travis-racket][tr] for Travis, it turns out it is not
too complex to setup build and test of a Racket package.

Before creating the actual pipelines, I find it simpler to setup the Racket
package project with the required files, and there are two of them: the
`azure-pipelines.yml` file controls the build, and `install-racket.sh` which
downloads and installs Racket.  These files don't need to be in the root
folder of the package, they can be anywhere (and the build YAML file can have
any name).  I prefer to place then (and any additional build files) in the
**ci** sub-folder of the package. For an example setup, see the
[plot-container][pc].

## azure-pipelines.yml -- the Build File

The `azure-pipelines.yml` file controls the build and test of the application,
and for a basic racket package can be really simple.  The example below should
work for any package as long as the `PACKAGE_NAME` variable is updated to
contain the actual package name:

```yaml
pool:
  vmImage: 'Ubuntu-16.04'
variables:
  PACKAGE_NAME: "plot-container"
  RACKET_VERSION: 7.2
  RACKET_DIR: $(Agent.BuildDirectory)/racket
  PLTSTDERR: 'error warning@optimizer'
steps:
- script: bash ./ci/install-racket.sh
  displayName: 'Install Racket $(RACKET_VERSION)'
- script: |
    PATH="$(RACKET_DIR)/bin:$(PATH)"
    raco pkg install --deps search-auto  --type dir --name $(PACKAGE_NAME)
    raco setup --check-pkg-deps --pkgs $(PACKAGE_NAME)
    raco test --no-run-if-absent --package $(PACKAGE_NAME)
  displayName: 'Build and Test $(PACKAGE_NAME)'
```

If you are familiar with Travis, the structure of the file should be easy to
understand:

* the `vmImage` tag selects the Virtual Machine to use -- I find Linux
  machines to be easiest to use, but see later in this blog post on how to use
  Windows machines too.
* the `variables` section defines a set of variables to use in the pipeline.
  In this case, the name of the package, racket version to install and where
  to install it.  I also prefer to set the `PLTSTDERR` variable so the Racket
  compiler will output some extra warnings, which help discovering problems
  early.
* the `steps` section contains the steps needed to build the package.  There
  are only two of them: installing racket and running the normal racket
  package build and setup commands.

Unlike Travis, Azure Pipelines will not check out the sources in a directory
the same as the package name, so the actual name of the package needs to be
explicitly specified to the `raco pkg` and `raco setup` so these commands work
correctly.

To build the package using different Racket versions, you can set up matrix
configurations for variables by adding a `strategy` section to the azure
pipelines files and specifying which variable combinations to use for
building.  In the example below, the package would be built with two jobs, one
for Racket 7.2 and one for Racket 7.1:

```yaml
strategy:
  maxParallel: 2
  matrix:
    Racket_7_2:
      RACKET_VERSION: 7.2
    Racket_7_1:
      RACKET_VERSION: 7.1
```

If the Racket package uses the `racket/gui` library during running the tests
(even if no GUI windows are opened), you will find that the above pipelines
file will fail to build, because no X Server is running by default on the
build machines.  The solution is to start the X Virtual Frame Buffer server,
`Xvfb`, as a separate build step early in the pipeline, than add a `DISPLAY`
variable indicating which connection the server is using:

```yaml
variables:
  DISPLAY: :99.0
  # other variables follow here
steps:
- script: (/usr/bin/Xvfb :99 -screen 0 1280x1024x24 &)
  displayName: "Start Xvfb for DISPLAY=$(DISPLAY)"
# other steps folow here
```

### Installing Racket

Racket is not installed on any of the virtual machines used for the builds, so
it has to be installed as part of the pipeline.  It is the simplest to do this
in a shell script which is invoked from the pipeline itself.  A basic shell
script is just a few lines of code, which downloads the installer file using
`curl` than runs it:

```sh
#!/bin/bash
set -e                         # fail script if any of the commands in it fail
RACKET_DIR=${RACKET_DIR:=~/racket}
RACKET_VERSION=${RACKET_VERSION:=7.2}
BASE="https://www.cs.utah.edu/plt/installers"

URL="${BASE}/${RACKET_VERSION}/racket-${RACKET_VERSION}-x86_64-linux.sh"
DFILE=$(mktemp ${TMP:-/tmp}/${0##**/}.XXXXXXXXXX)

echo "Downloading $URL ..."
curl --output $DFILE $URL
echo "Installing Racket into $RACKET_DIR ..."
/bin/bash $DFILE --in-place --dest "$RACKET_DIR"
```

The above script will install the latest Racket version (7.2 at the time of
this writing), but it also allows controlling the actual version using the
`RACKET_VERSION` environment variable and the place where Racket is installed
using the `RACKET_DIR` environment variable.  Values for these variables can
be specified in the Azure Pipelines build files, allowing installation of
different versions without modifying this script.

The above script will work for recent Racket versions (7.0 onwards).  For more
complex installation shell scripts, which also handle older Racket versions,
see the [install-racket.sh][ghir] script that is part of the `travis-racket`
repository, and I also wrote my [own version][ahir], mostly as an exercise in
writing terse shell scripts.  Both these shell scripts can be used in Azure
Pipelines but they are somewhat complex to fit in a blog post, since they
handle installation of old versions plus some extra checking.

Also, at the time of writing this post, the main Racket mirror site was down,
so the download script uses the UTAH site directly.  Normally, the `BASE`
download URL should be set to https://mirror.racket-lang.org/installers.

## Setup the build pipeline

You'll need an [Azure DevOps][azuredevops] account and create a project to
hold the build pipelines.  A single project can hold pipelines for several
GitHub projects -- this is really convenient if you only want to use DevOps
for the pipeline functionality, because you only need to create one project.

There are several on-line tutorials on how to setup and create the actual
build pipeline, so this is just a short bullet point list of what needs to be
done:

* Create an Azure DevOps account, if you don't already have one
* Install Azure Pipelines from the GitHub MarketPlace and authorize it for
  your GitHub accont
* Create a new Azure Pipelines project to hold the build pipelines
* Create a new build pipeline -- you will be prompted to select where the
  project is located and where the build YAML file is withing the project,
  since the file does not have to be in the root of your project

## Beyond the basics

There are several other Azure Pipelines features which are useful in building
more complex applications -- for such an example, see the build pipeline for
the [ActivityLog2 project][al2az].  This is a pipeline which can build a
Racket application on both Windows and Linux (and there is an
[install-racket.ps1][ahirw] which downloads installs Racket on Windows),
download test databases from an external server, build an application with
embedded API keys, which are stored as secret variables and publish the
resulting installer as a build artifact.  For more details about this process,
see that builds [README.md][al2br] file.

[ghir]: https://github.com/greghendershott/travis-racket/blob/master/install-racket.sh
[ahir]: https://github.com/alex-hhh/plot-container/blob/master/ci/install-racket.sh
[ci]: https://en.wikipedia.org/wiki/Continuous_integration
[tr]: https://github.com/greghendershott/travis-racket/
[azuredevops]: https://dev.azure.com
[pc]: https://github.com/alex-hhh/plot-container
[al2az]: https://github.com/alex-hhh/ActivityLog2/blob/master/etc/scripts/azure-pipelines.yml
[ahirw]: https://github.com/alex-hhh/ActivityLog2/blob/master/etc/scripts/install-racket.ps1
[al2br]: https://github.com/alex-hhh/ActivityLog2/tree/master/etc/scripts
