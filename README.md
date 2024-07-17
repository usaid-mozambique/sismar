# sismar <a href="https://usaid-mozambique.github.io/sismar/"><img src="man/figures/logo.png" align="right" height="120" alt="sismar website" /></a>

Create analytic datasets from MISAU tabular data

## Overview

Efficient analysis of tabular data exported from MISAU health information systems requires munging actions including pivoting, removal/coercion of variables, and engineering of data features useful in analysis. To this end, the sismar package provides a set of functions and wrappers helpful in preparing analytic datasets for use within the R environment or for export and use in alternative analysis solutions.


## Installation

`sismar` is not on CRAN, so users will have to install it directly from [GitHub](https://github.com/usaid-mozambique/) using the below code.

``` r

    ## SETUP

    # install from GitHub using remotes
      install.packages("remotes")
      remotes::install_github("usaid-mozambique/sismar")
    
    # load the package
      library(sismar)
      
    ## LIST FUCTIONS INCLUDED WITH PACKAGE
      ls("package:sismar")
    
```
