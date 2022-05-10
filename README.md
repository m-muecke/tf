
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tf <img src="https://github.com/tidyfun/tidyfun/blob/master/README_files/figures/tidyfun_hex.gif?raw=true" align="right" height = "150" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R build
status](https://github.com/tidyfun/tf/workflows/R-CMD-check/badge.svg)](https://github.com/tidyfun/tf/actions)
[![codecov.io](https://codecov.io/github/tidyfun/tf/coverage.svg?branch=main)](https://codecov.io/github/tidyfun/tf/branch/main)
[![MIT
license](http://img.shields.io/badge/license-MIT-brightgreen.svg)](https://opensource.org/licenses/MIT)
<!-- badges: end -->

The goal of **`tidyfun`** is to provide accessible and well-documented
software that **makes functional data analysis in `R` easy** –
specifically data wrangling and exploratory analysis. This package
**`tf`** provides the infrastructure for **`tidyfun`** with minimal
dependencies – specifically, no `tidyverse`-dependencies.

`tf` includes definitions of the necessary new data types (`tf`) and
associated methods. Vectors of class `tf` can be operated on using many
standard functions (`+`, `mean`, etc.) as well as several new functions
in `tf` (`tf_smooth`, `tf_where`).

**Crucially**, vectors of class `tf` can be included in data frames
containing other variables, enabling simple data manipulation. This
approach is connected to the conceptual framework in functional data
analysis which assumes that *complete functions* are the unit of
observation. With `tidyfun` and `tf`, full curves sit alongside numeric,
factor, and other observations on the same subject.

## Installation

``` r
devtools::install_github("tidyfun/tf")
```

## Overview

**`tidyfun`** provides:

-   new **data types** for representing functional data: **`tfd`** &
    **`tfb`**
-   arithmetic **operators** and descriptive **statistics** for such
    data
-   basic **graphics** functions for `tf` vectors

Please see the [`tidyfun` website](https://tidyfun.github.io/tidyfun)
for the full documentation.

## What does it do?

#### New vector-like data types for functional data

**`tf`** provides [new `S3`-classes for functional
data](https://tidyfun.github.io/tidyfun/reference/index.html#section-tf-sub-classes-constructors-converters),
either as raw data (class `tfd` for *t*idy *f*unctional *d*ata) or in
basis representation (class `tfb` for *t*idy *f*unctional *b*asis data).

Such `tf`-objects can be subsetted or subassigned, computed on and
summarized.

Almost all

-   operators like `==`, `+` or `*`
-   math functions like `sum`, `log` or `abs`
-   and statistics functions like `mean` or `sd`

are defined for **`tf`**’s data structures
([more](https://tidyfun.github.io/tidyfun/reference/index.html#section-arithmetic-logical-and-summary-functions)).

The `tf` objects are basically glorified lists, so they work well as
columns in data frames. That makes it a lot easier to keep your other
data and functional measurements together in one object for
preprocessing, exploratory analysis and description. At the same time,
these objects actually behave like vectors of *functions* to some
extent, i.e., they can be evaluated on any point in their domain, they
can be integrated or differentiated, etc.

[See
here](https://tidyfun.github.io/tidyfun/articles/x01_tf_Vectors.html)
for more information on the operations defined for `tf` vectors.

#### Methods for converting existing data to `tf` and back

**`tf`** includes functions `tfd` and `tfb` for converting matrices,
data frames, etc. to `tf` vectors and back. More data wrangling
functionality in a `tidyverse`-inspired way is available from `tidyfun`.

[See
here](https://tidyfun.github.io/tidyfun/articles/x02_Conversion.html)
for details on getting data into (and out of) the `tf` format.

------------------------------------------------------------------------

Found a bug? Got a question? Missing some functionality?  
Please let us know so we can make it better.
