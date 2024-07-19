# riskmetric

<details>

* Version: 0.2.4
* GitHub: https://github.com/pharmaR/riskmetric
* Source code: https://github.com/cran/riskmetric
* Date/Publication: 2024-01-09 15:50:02 UTC
* Number of recursive dependencies: 111

Run `revdepcheck::revdep_details(, "riskmetric")` for more info

</details>

## Newly broken

*   R CMD check timed out
    

## Newly fixed

*   checking running R code from vignettes ...
    ```
      ‘extending-riskmetric.Rmd’ using ‘UTF-8’... OK
      ‘riskmetric.Rmd’ using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘riskmetric.Rmd’
      ...
    > options(repos = "https://cran.rstudio.com")
    
    > knitr::opts_chunk$set(collapse = TRUE, comment = "#>", 
    +     fig.path = "man/figures/")
    
    > knitr::include_graphics("../man/figures/core-workflow.svg")
    
      When sourcing ‘riskmetric.R’:
    Error: Cannot find the file(s): "../man/figures/core-workflow.svg"
    Execution halted
    ```

