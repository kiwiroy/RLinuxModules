
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RLinuxModules

<!-- badges: start -->

[![R build
status](https://github.com/kiwiroy/RLinuxModules/workflows/R-CMD-check-basic/badge.svg)](https://github.com/kiwiroy/RLinuxModules/actions)
<!-- badges: end -->

R package that makes linux [environment
modules](http://modules.sourceforge.net/) available from R.

## Installation

<s>You can install the released version of RLinuxModules from
[CRAN](https://CRAN.R-project.org) with:</s>

``` r
# Not released to CRAN
# install.packages("RLinuxModules")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("kiwiroy/RLinuxModules@main")
```

## Example

This is a basic example which shows you how to solve a common problem
when using environment modules with R:

``` r
library(RLinuxModules)

# initialise
moduleInit(modulesHome = modulesHome)

# load samtools into the environment
module("load samtools") 

# samtools should now be available
system("samtools", intern = TRUE)
```

While in a knitr code chunk using `bash` as the engine.

``` bash
# Environment is inherited in other code chunks.
which samtools
```

``` bash
# see known issues below
module list
```

## How it works

The Modules Environment now supports R scripting since version 4.0.
<s>This package works by using the python support and translating the
python commands returned from *modulecmd python* into R commands. It has
only been tested for version 3.2.10</s>

### Known issues

1.  If `/bin/sh` is symlinked to `/bin/dash`, `dash` will sanitize the
    environment in such a way that the `module` function is not
    available to the child `bash` shell. See a [bug
    report](https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=814358)
    and [Stack Overflow question](https://stackoverflow.com/q/38079864)
    on the topic.
