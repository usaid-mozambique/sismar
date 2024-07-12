# sismar <a href="https://usaid-mozambique.github.io/sismar/"><img src="man/figures/logo.png" align="right" height="120" alt="sismar website" /></a>

Transform tabular export files from SISMA

## Overview

When using SISMA data, the OHA SI PEPFAR by and large uses the same MER Structured Datasets to answer the same analytical questions each period. This package is a sister package of `ICPIutilities` for working primiarly with data from DATIM and the MER Structured Datasets and plotting them using `glitr`. Focal users are analysts in USAID/GH/OHA who are using R to pull data from DATIM or perform the same repeated functions each quarter like creating TX_NET_NEW targets or assessing achievement.


## Installation

`sismar` is not on CRAN, so you will have to install it directly from [GitHub](https://github.com/usaid-mozambique/) using the code found below..

```{r}
remotes::install_github("usaid-mozambique/sismar")
```
