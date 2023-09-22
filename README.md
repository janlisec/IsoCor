
# IsoCor

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/IsoCor)](https://CRAN.R-project.org/package=IsoCor)
[![R-CMD-check](https://github.com/janlisec/IsoCor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/janlisec/IsoCor/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/janlisec/IsoCor/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/janlisec/IsoCor/actions/workflows/test-coverage.yaml)
<!-- badges: end -->

The goal of IsoCor is to analyze isotope data from ICP-MS analyses using a 
dedicated `Shiny`-App. Two workflows are supported for IR and IDMS calculation.

You can access and test the app without local installation at [www.bam.de/IsoCor](https://www.bam.de/IsoCor).

## Installation

To install the development version of `IsoCor` from [GitHub](https://github.com/) use:

``` r
devtools::install_github("janlisec/IsoCor")
```

or alternatively install from CRAN:

``` r
install.packages("IsoCor")
```

## Example

Afterwards you can start the app locally in your browser running:

``` r
IsoCor::ic_app()
```

The app contains test-data sets for both workflows.

## Detailed documentation

A publication for the IR workflow can be found [here](https://doi.org/10.1039/D2JA00208F). 
A publication for the IDMS workflow is currently under review. 
