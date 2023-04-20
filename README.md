
<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- badges: start -->

[![License: GPL
v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[![R build
status](https://github.com/PolMine/bignlp/workflows/R-CMD-check/badge.svg)](https://github.com/PolMine/bignlp/actions)
[![codecov](https://codecov.io/gh/PolMine/bignlp/branch/master/graph/badge.svg)](https://codecov.io/gh/PolMine/bignlp/branch/master)
[![Project Status: Active – The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

## The ‘bignlp’-package: Objectives

R users who want to add a linguistic annotation to a corpus are already
offered a few available packages for Natural Language Processing (NLP):

- [openNLP](https://CRAN.R-project.org/package=openNLP)
- [cleanNLP](https://CRAN.R-project.org/package=cleanNLP)
- [coreNLP](https://CRAN.R-project.org/package=coreNLP)
- [spacyr](https://CRAN.R-project.org/package=spacyr)
- [udpipe](https://CRAN.R-project.org/package=udpipe)

These packages are not “pure R” NLP-tools. They offer interfaces to
standard NLP tools implemented in other programming languages. The
[cleanNLP](https://CRAN.R-project.org/package=cleanNLP) R package offers
to combine these external tools in one coherent framework. So why yet
another NLP R package?

The listed packages are not good to deal with large volumes of text as a
matter of performance and memory efficiency. The bignlp R package offers
fast, robust and convenient functionality for annotating large-scale
corpora, including XML data formats. It uses [Stanford
CoreNLP](https://stanfordnlp.github.io/CoreNLP/) as standard,
high-quality tool in parallel mode. To be parsimonious with the memory
available, it implements line-by-line processing, so that annotated data
is not be kept in memory. The bignlp package may be your package of
choice you if your data is substantially big.

## Installation and dependencies

The bignlp package itself is a pure R package. It does not include
compiled code, and the installation of the package itself is unlikely to
cause problems. At this stage, bignlp is a GitHub-only package. Using an
installation mechanism of the [remotes]() package exported by
[devtools]() you can install bignlp as follows.

``` r
devtools::install_github("PolMine/bignlp")
```

However bignlp depends on:

- The Java library [Stanford
  CoreNLP](https://stanfordnlp.github.io/CoreNLP/)
- The availability of a Java Virtual Machine. Java 1.8 is recommended.
- [rJava](https://CRAN.R-project.org/package=rJava)

### rJava and Java

Install the rJava R package:

``` r
install.packages("rJava")
```

Stanford CoreNLP officially requires Oracle Java v1.8+, see [the CoreNLP
Website](https://stanfordnlp.github.io/CoreNLP/). Using CoreNLP in
combination with OpenJDK, a Java flavour that is available on many
systems, is not officially supported. Yet we are not aware of errors
resulting from using the “wrong” Java. If you see errors, you might want
to ensure that you have the “correct”” Java (Java Development Kit / JDK)
installed.

Oracle Java can be installed from [the Oracle
website](https://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html).
Note that Oracle licenses have become more restrictive, and registration
may be necessary. This will not be a problem for academic/non-commercial
contexts.

Once downloading and installing Oracle Java 1.8+ is completed, making
this Java version the one that is actually used may be tricky. For
macOS, we found the jEnv tool to be useful, see [this
link](https://medium.com/@danielnenkov/multiple-jdk-versions-on-mac-os-x-with-jenv-5ea5522ddc9b).

To get R use Java 1.8, you may finalle have to call R CMD javareconf
from the command line.

``` sh
R CMD javareconf
```

To check within R that everything works, run this snippet from within R.

``` r
library(rJava)
.jinit()
.jcall("java/lang/System", "S", "getProperty", "java.runtime.version")
```

The output you see (something like “1.8.0_211-b12”) on your system
depend on the Java version you have installed.

### Installing CoreNLP

CoreNLP is a Java package that needs to be present on your machine.

A conventional place for installing a tool such as CoreNLP on Linux and
macOS is the /opt dir. So from a terminal, create a directory for
CoreNLP, go into it, download the zipped jar files, unzip it, and remove
the zip file. Note that sudo rights may be necessary to have permissions
to write into the /opt directory.

``` sh
CORENLP_LATEST=`Rscript -e 'bignlp::corenlp_latest()'|grep -oE '\d+\.\d+\.\d+'`
```

``` sh
mkdir /opt/stanford-corenlp
cd /opt/stanford-corenlp
wget http://nlp.stanford.edu/software/stanford-corenlp-$CORENLP_LATEST.zip
unzip stanford-corenlp-$CORENLP_LATEST.zip
rm stanford-corenlp-$CORENLP_LATEST.zip
```

You may want to to install a language model for English.

``` sh
cd /opt/stanford-corenlp/stanford-corenlp-$CORENLP_LATEST
wget http://nlp.stanford.edu/software/stanford-corenlp-$CORENLP_LATEST-models-english.jar
```

We illustrate getting models for a specific language for German. We go
into the CoreNLP directory and download the model to this place.

``` sh
cd stanford-corenlp-$CORENLP_LATEST
wget http://nlp.stanford.edu/software/stanford-corenlp-$CORENLP_LATEST-models-german.jar
```

The jar file with the model include a default properties file for
processing German data (StanfordCoreNLP-german.properties). You can see
this by displaying the content of the jar as follows.

``` sh
jar tf stanford-corenlp-$CORENLP_LATEST-models-german.jar | grep "properties" # see content of jar
```

The inclusion of this properties file in the jar may become a problem.
If you want to configure the parser yourself, you may encounter the
issue that this properties file included in the jar will override any
other properties you may want to use.

The solution we found to work is to (a) extract the properties file from
the jar and (b) remove it from the jar.

``` sh
# extract StanfordCoreNLP-german.properties from jar
unzip stanford-corenlp-$CORENLP_LATEST-models-german.jar StanfordCoreNLP-german.properties
# remove StanfordCoreNLP-german.properties from jar
zip -d stanford-corenlp-$CORENLP_LATEST-models-german.jar StanfordCoreNLP-german.properties
```

Note that the bignlp package already includes a properties file that has
been edited for annotating large amounts of data quickly. You will find
it as follows:

``` r
options(bignlp.properties_file = corenlp_get_properties_file(lang = "de", fast = TRUE))
```

## Core Functionality

``` r
library(bignlp)
```

    ## CoreNLP jar directory: /opt/stanford-corenlp/stanford-corenlp-4.4.0

    ## CoreNLP version: v4.4.0

    ## JVM memory allocated: 491 Mb

    ## JVM file encoding: UTF-8

``` r
library(data.table)

reuters_file <- system.file(package = "bignlp", "extdata", "txt", "reuters.txt")
reuters_txt <- readLines(reuters_file)
reuters_tab <- data.table(
  doc_id = 1L:length(reuters_txt), 
  text = reuters_txt
)

props <- corenlp_get_properties_file(lang = "en", fast = "TRUE")

y <- corenlp_annotate(
  x = reuters_tab,
  pipe = props,
  corenlp_dir = corenlp_get_jar_dir(),
  progress = FALSE
)
```

    ## → JVM runtime name: OpenJDK Runtime Environment

    ## → JVM version: 1.8.0_332-b09

    ## ! Recommended: Oracle Java 8

    ## ! JVM maximum heap space: 491 Mb - recommended: 4 GB

## Known issues

### rJava

An issue we have seen on macOS in combination with RStudio is that the
rJava dynamic library called from the RStudio session does not link
correctly against the dynamic library of the Java virtual machine. We
use the follwing snippet to induce the correct link. (Note that paths
may have to be updated to reflect the correct R and Java version!)

``` sh
install_name_tool -change "@rpath/libjvm.dylib" \
  "/Library/Java/JavaVirtualMachines/jdk1.8.0_211.jdk/Contents/Home/jre/lib/server/libjvm.dylib" \
  /Library/Frameworks/R.framework/Versions/3.5/Resources/library/rJava/libs/rJava.so
```

### Issues with RStudio

We have seen problems with the parallel annotation with from an RStudio
session. Running bignlp from a simple terminal window is a good
solution. For a large annotation project, omitting an IDE may be a good
idea anyway.
