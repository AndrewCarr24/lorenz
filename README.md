
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lorenz

<!-- badges: start -->

[![Travis build
status](https://api.travis-ci.com/datadiarist/lorenz.svg?branch=master)](https://api.travis-ci.com/datadiarist/lorenz.svg?token=FKe5Y6rhxscJvKHWjpxN&branch=master)
<!-- badges: end -->

## Overview

The U.S. Census publishes income data as counts in income brackets.
Estimating income inequality from grouped income data requires that
certain assumptions be made about the distribution of incomes within
these brackets. The lorenz package provides two methods for computing
income inequality statistics using grouped income data.

The package consists of main functions - lorenz\_interp and mcib.
lorenz\_interp, which stands for Lorenz interpolation, is based on a new
method I developed that estimates income inequality using an
interpolated Lorenz curve. mcib, which stands for mean-constrained
integration over brackets, estimates income inequality using a technique
described in a 2018 paper from Jargowsky and Wheeler.

## Installation

You can currently install the development version of lorenz from Github.

``` r
# install.packages("devtools")
devtools::install_github("datadiarist/lorenz")
```

## Usage

The main function of the lorenz package is lorenz\_interp. This function
requires three arguments - a vector giving the number of
people/households in each bracket, a vector giving the bracket
boundaries, and a number specifying the income distribution mean.
Following previous research, the distribution described by the income
boundaries is assumed to be bounded at the bottom and unbounded at the
top. For instance, the boundaries in the example below specify a
distribution whose bottom income bracket is $0-10000 and whose top
income bracket is $200000+.

``` r
library(lorenz)

frequencies <- c(45, 31, 33, 27, 43, 40, 51, 50, 63, 97, 121, 132, 64, 54, 32, 12)
boundaries <- c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 60000, 75000,
100000, 125000, 150000, 200000)
mean_income <- 66500

# The Gini coefficient is given by default 
lorenz_interp(frequencies, boundaries, mean_income)
#> [1] 0.3707927

# Use the stat parameter to specify the inequality measure (gini, theil, or atkinson)
lorenz_interp(frequencies, boundaries, mean_income, stat = 'theil')
#> [1] 0.238408
```

lorenz also provides an implementation of mean-constrained integration
over brackets with the mcib function. This function also requires three
parameters - frequencies, boundaries, and a distribution mean.

``` r
# MCIB Gini
mcib(frequencies, boundaries, mean_income)
#> [1] 0.3584898

# MCIB Theil
mcib(frequencies, boundaries, mean_income, stat = 'theil')
#> [1] 0.2342135
```

## Help

If you encounter a bug, please file an issue with a reproducible example
on [GitHub](https://github.com/datadiarist/lorenz/issues).
