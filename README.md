# SAVI Monkeypox 
<!-- badges: start -->
[![R-CMD-check](https://github.com/CDCGov/savimpx/workflows/R-CMD-check/badge.svg)](https://github.com/CDCGov/savimpx/actions)
[![Codecov test coverage](https://codecov.io/gh/CDCgov/savimpx/branch/master/graph/badge.svg)](https://app.codecov.io/gh/CDCgov/savimpx?branch=master)
<!-- badges: end -->

## About

This package contains some helpful R functions for data connection, transformation, and visualization to support CDC's International MPX response.  
Most functions are fairly generic, CDC-specific, and liable to change drastically as needs shift. 

## Installation

You can install the latest development version of savimpx from GitHub using the following command:

```r
devtools::install_github("CDCGoV/savimpx")
```

## Thanks and Shoutouts

- Thanks to the [{spud}](https://github.com/reside-ic/spud.git) R package developed by RESIDE @ Imperial College London
  - Sharepoint configuration at CDC prohibited us from using {spud} natively, but our handling of the Request Digest was heavily inspired by their approach.