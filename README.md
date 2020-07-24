<!-- badges: start -->
[![Travis build status](https://travis-ci.com/datadiarist/lorenz.svg?branch=master)](https://travis-ci.com/datadiarist/lorenz)
<!-- badges: end -->

# lorenz 

lorenz is an R package that provides two methods for computing income inequality statistcs from grouped income data.  This is usually the format in which income data is published.

The package consists of two main functions - mcib and lorenz_interp.  mcib, which stands for mean-constrained integration over brackets, is an implementation of a technique described in Jargowsky and Wheeler's 2018 paper.  lorenz_interp, which stands for Lorenz interpolation, is based on a new method I developed that computes income statistics from sampled exact incomes based on an interpolated Lorenz curve.  


## References 

Jargowsky, Paul A., and Christopher A. Wheeler. 2018. “Estimating Income Statistics from Grouped Data: Mean-Constrained Integration over Brackets.” Sociological Methodology 48(1):337–74.



