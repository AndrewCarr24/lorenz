
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lorenz

<!-- badges: start -->

[![Travis build
status](https://travis-ci.com/datadiarist/lorenz.svg?branch=master)](https://travis-ci.com/datadiarist/lorenz.svg?token=FKe5Y6rhxscJvKHWjpxN&branch=master)
<!-- badges: end -->

## Overview

The lorenz package provides two methods for computing income inequality
statistics from grouped income data. This is the format in which the
U.S. Census publishes income data.

The package consists of two main functions - mcib and lorenz\_interp.
mcib, which stands for mean-constrained integration over brackets,
estimates income inequality using a technique described in a 2018 paper
from Jargowsky and Wheeler. lorenz\_interp, which stands for Lorenz
interpolation, is based on a new method I developed that estimating
income inequality using an interpolated Lorenz curve.

## Installation

You can currently install the development version of lorenz from Github.

``` r
# install.packages("devtools")
devtools::install_github("datadiarist/lorenz")
```

## Usage

``` r
library(lorenz)

frequencies <- c(45, 31, 33, 27, 43, 40, 51, 50, 63, 97, 121, 132, 64, 54, 32, 12)
boundaries <- c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 60000, 75000,
100000, 125000, 150000, 200000)
mean <- 66500

lorenz_interp(frequencies, boundaries, mean)
#> [1] 0.3707927
```

## Help

If you encounter a bug, please file an issue with a reproducible example
on [GitHub](https://github.com/datadiarist/lorenz/issues).
