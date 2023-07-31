
# IsoCor

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/IsoCor)](https://CRAN.R-project.org/package=IsoCor)
[![test-coverage](https://github.com/janlisec/IsoCor/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/janlisec/IsoCor/actions/workflows/test-coverage.yaml)
[![R-CMD-check](https://github.com/janlisec/IsoCor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/janlisec/IsoCor/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of IsoCor is to analyze isotope data from ICP-MS analyses using a 
dedicated `Shiny`-App. Two workflows are supported for IR and IDMS calculation.

## Installation

You can install the development version of `IsoCor` from 
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("janlisec/IsoCor")
install.packages("IsoCor")
```

## Example

You can start the app locally in your browser running:

``` r
library(IsoCor)
ic_app()
```
