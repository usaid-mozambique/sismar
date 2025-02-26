# sismar <a href="https://usaid-mozambique.github.io/sismar/"><img src="man/figures/logo.png" align="right" height="200" alt="sismar website" /></a>

<!-- badges: start -->
[![R-CMD-check](https://github.com/usaid-mozambique/sismar/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/usaid-mozambique/sismar/actions/workflows/R-CMD-check.yaml)
[![Lifecycle: stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![sismar status badge](https://usaid-mozambique.r-universe.dev/badges/sismar)](https://usaid-mozambique.r-universe.dev/sismar)
[![:name status badge](https://usaid-mozambique.r-universe.dev/badges/:name)](https://usaid-mozambique.r-universe.dev/)
<!-- badges: end -->

Criar quadros de dados analíticos a partir de dados brutos do MISAU

## Resumo

A análise eficiente de dados rectangulares exportados dos sistemas de informação de saúde do MISAU requer acções de processamento tais como pivotagem, eliminação/coerção de variáveis e engenharia de caraterísticas de dados úteis na análise. O pacote sismar fornece um conjunto de funções que executam essas acções e preparam os quadros de dados para análise no R ou em software analítico alternativo.


## Instalação

`sismar` não está alojado no CRAN, Eventuais usarios podem instalá-lo directamente de rOpenSci usando o código abaixo.

``` r

    ## CONFIGURAÇÃO

    # instalar a partir de rOpenSci
      install.packages("sismar", repos = c('https://usaid-mozambique.r-universe.dev', 'https://cloud.r-project.org'))
    
    # carregar o pacote
      library(sismar)
      
    ## LISTAR FUNÇÕES INCLUÍDAS NO PACOTE
      ls("package:sismar")
    
```

---

*Disclaimer: The findings, interpretation, and conclusions expressed herein are those of the authors and do not necessarily reflect the views of United States Agency for International Development. All errors remain our own.*
