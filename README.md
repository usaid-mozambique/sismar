# sismar <a href="https://usaid-mozambique.github.io/sismar/"><img src="man/figures/logo.png" align="right" height="120" alt="sismar website" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/usaid-mozambique/sismar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/usaid-mozambique/sismar/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![:name status badge](https://usaid-mozambique.r-universe.dev/badges/:name)](https://usaid-mozambique.r-universe.dev/)
[![sismar status badge](https://usaid-mozambique.r-universe.dev/badges/sismar)](https://usaid-mozambique.r-universe.dev/sismar)
<!-- badges: end -->

Criar quadros de dados analíticos a partir de dados brutos do MISAU

## Overview

A análise eficiente de dados rectangulares exportados dos sistemas de informação de saúde do MISAU requer acções de processamento tais como pivotagem, eliminação/coerção de variáveis e engenharia de caraterísticas de dados úteis na análise. O pacote sismar fornece um conjunto de funções que executam essas acções e preparam os quadros de dados para análise no R ou em software analítico alternativo.


## Installation

`sismar` não está no CRAN, então os usuários terão que instalá-lo directamente do GitHub usando o código abaixo.

``` r

    ## SETUP

    # install from rOpenSci
      install.packages("sismar", repos = c('https://usaid-mozambique.r-universe.dev', 'https://cloud.r-project.org'))
    
    # load the package
      library(sismar)
      
    ## LIST FUNCTIONS INCLUDED WITH PACKAGE
      ls("package:sismar")
    
```

---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*
