<!-- badges: start -->
[![Travis build status](https://travis-ci.com/datadiarist/lorenz.svg?branch=master)](https://travis-ci.com/datadiarist/lorenz.svg?token=FKe5Y6rhxscJvKHWjpxN&branch=master)
<!-- badges: end -->

# lorenz 

## Overview

The lorenz package provides two methods for computing income inequality statistics from grouped income data.  This is the format in which the U.S. Census publishes income data.

The package consists of two main functions - mcib and lorenz_interp.  mcib, which stands for mean-constrained integration over brackets, estimates income inequality using a technique described in a 2018 paper from Jargowsky and Wheeler.  lorenz_interp, which stands for Lorenz interpolation, is based on a new method I developed that estimating income inequality using an interpolated Lorenz curve.  

## Installation

You can currently install the development version of lorenz from Github.

``` r
# install.packages("devtools")
devtools::install_github("datadiarist/lorenz")
```

## References 

Jargowsky, Paul A., and Christopher A. Wheeler. 2018. “Estimating Income Statistics from Grouped Data: Mean-Constrained Integration over Brackets.” Sociological Methodology 48(1):337–74.



