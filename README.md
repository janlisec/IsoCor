# IsoCor

<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/IsoCor)](https://CRAN.R-project.org/package=IsoCor)
[![](https://img.shields.io/badge/devel%20version-0.2.8-blue.svg)](https://github.com/janlisec/isocor)
[![R-CMD-check](https://github.com/janlisec/IsoCor/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/janlisec/IsoCor/actions/workflows/R-CMD-check.yaml)
[![test-coverage](https://github.com/janlisec/IsoCor/actions/workflows/test-coverage.yaml/badge.svg)](https://github.com/janlisec/IsoCor/actions/workflows/test-coverage.yaml)
[![](http://cranlogs.r-pkg.org/badges/grand-total/IsoCor?color=grey)](https://cran.r-project.org/package=IsoCor)
[![](https://img.shields.io/badge/doi-10.1039/D2JA00208F-yellow.svg)](https://doi.org/10.1039/D2JA00208F)
[![](https://img.shields.io/badge/doi-10.1021/acs.analchem.3c03553-yellow.svg)](https://doi.org/10.1021/acs.analchem.3c03553)
<!-- badges: end -->

The goal of IsoCor is to analyze isotope data from Inductively-coupled-plasma
Mass-spectrometry (ICP-MS) analyses using a dedicated `Shiny`-App. 
Two workflows are supported for isotope ratio (IR) and isotope dilution (ID) 
calculation.

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

![App screenshot](dev/Screenshot.png?raw=true "Screenshot")

The app contains test-data sets for both workflows, IR and ID. Measurement files
can be uploaded in a number of different formats. Most parameters are explained 
either by tooltips or popups.

## Detailed documentation

Scientific publications for the IR and ID workflows can be found [here](https://doi.org/10.1039/D2JA00208F)
and [here](https://doi.org/10.1021/acs.analchem.3c03553). 
