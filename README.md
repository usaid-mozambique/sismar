# sismar <a href="https://usaid-mozambique.github.io/sismar/"><img src="man/figures/logo.png" align="right" height="120" alt="sismar website" /></a>

Create analytic dataframes from MISAU rectangular data
## TEST
## Overview

Efficient analysis of rectangular data exported from MISAU health information systems requires munging actions such as pivoting, removal/coercion of variables, and engineering of data features useful in analysis. The sismar package provides a set of functions that perform such actions and prepare dataframes for analysis in R or alternative analytic software.


## Installation

`sismar` is not on CRAN, so users will have to install it directly from [GitHub](https://github.com/usaid-mozambique/) using the code below.

``` r

    ## SETUP

    # install from GitHub using remotes
      install.packages("remotes")
      remotes::install_github("usaid-mozambique/sismar")
    
    # load the package
      library(sismar)
      
    ## LIST FUNCTIONS INCLUDED WITH PACKAGE
      ls("package:sismar")
    
```
