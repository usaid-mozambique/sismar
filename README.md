# sismar <a href="https://usaid-mozambique.github.io/sismar/"><img src="man/figures/logo.png" align="right" height="120" alt="sismar website" /></a>

Transform tabular export files from SISMA

## Overview

Efficient analysis of SISMA data exports requires transformation actions applied to the downloaded tabular data.  This includes pivoting from a wide to long format, clean up the data frame (e.g. removal of unnecessary variables, modification of data types, etc.) and engineering of features useful in analysis.  This package provides a set functions and wrappers that perform these standardized actions generating data frames that analysts can use either within the R environment or export for analysis in other visualization solutions.  


## Installation

`sismar` is not on CRAN, so users will have to install it directly from [GitHub](https://github.com/usaid-mozambique/) using the code found below..

```{r}
## SETUP

    # install from GitHub using remotes
      install.packages("remotes")
      remotes::install_github("usaid-mozambique/sismar")
    
    # load the package
      library(sismar)
      
## LIST FUCTIONS INCLUDED WITH PACKAGE
    ls("package:sismar")
```
