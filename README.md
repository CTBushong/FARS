
# FARS

<!-- badges: start -->
  [![Travis build status](https://travis-ci.com/CTBushong/FARS.svg?branch=master)](https://travis-ci.com/CTBushong/FARS)
  <!-- badges: end -->


Functions fusing DOT Fatality Analysis Reporting System data for years 2013 - 2015.


## Installation

Run

``` r
if(!requireNameSpace("devtools"))
  install.packages("devtools")

devtools::install_github(repo = "CTBushong/FARS", build_vignettes = TRUE)
```

## For more on how to use this package

For more info on how to use this package please refer to the vignette by running:

``` r
library(fars)
vignette("fars-usage", package = "fars")
```

