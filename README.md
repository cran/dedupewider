
<!-- README.md is generated from README.Rmd. Please edit that file -->

# dedupewider <img src='man/figures/logo.png' align="right" height="199" />

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/dedupewider)](https://CRAN.R-project.org/package=dedupewider)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/grand-total/dedupewider?color=blue)](https://r-pkg.org/pkg/dedupewider)
<!-- badges: end -->

Duplicated data can exist in different rows and columns and user may
need to treat observations (rows) connected by duplicated data as one
observation, e.g. companies can belong to one family (and thus: be one
company) by sharing some telephone numbers. This package provides a
function to find connected rows based on data on chosen columns and
collapse it into one row.

Function from this package was used in CATI surveys (especially on
businesses databases) to minimize the chance that interviewers will call
independently the same respondent and thus irritate her or him. It is a
chance that the same, suitable person to participate in the survey,
works in more than one company and that these companies exist as a
separate records in the database (sometimes just as a separate
companies, sometimes as a branches). When trying to find participant in
company X, interviewer can be switched to company C to speak with
employee E and the second interviewer, when calling company Y, can also
be switched to company C to speak with employee E. If some data in
database (like phone numbers) can be use to collapse companies X, Y and
C into one record, the chance for this inconvenience will be much lower.

## Installation

You can install the released version of dedupewider from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("dedupewider")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("gsmolinski/dedupewider")
```

## Usage

``` r
library(dedupewider)

initial_table <- data.frame(tel_1 = c(111, 222, 444, 555),
                            tel_2 = c(222, 666, 666, 555),
                            tel_3 = c(NA, NA, NA, 555),
                            tel_4 = c(NA, NA, NA, 555),
                            tel_5 = c(NA, NA, NA, 555),
                            name = paste0("name", 1:4),
                            nace = c("01.19", "01.64", "55.90", "09.10"))

# Rows 1, 2 and 3 share the same phone number and thus will be collapsed into one row
# In row 4 the same phone number will be removed to leave only unique entry (555)
initial_table

#>   tel_1 tel_2 tel_3 tel_4 tel_5  name  nace
#> 1   111   222    NA    NA    NA name1 01.19
#> 2   222   666    NA    NA    NA name2 01.64
#> 3   444   666    NA    NA    NA name3 55.90
#> 4   555   555   555   555   555 name4 09.10

table_deduplicated <- dedupe_wide(initial_table, cols_dedupe = paste0("tel_", 1:5),
                                  cols_expand = "name")
table_deduplicated

#>   tel_1....1 tel_1....2 tel_1....3 tel_1....4 name....1 name....2 name....3  nace
#> 1        111        222        444        666     name1     name2     name3 01.19
#> 2        555         NA         NA         NA     name4      <NA>      <NA> 09.10
```

Please refer to a vignette for step-by-step explanation as well as
information about algorithm used: [Usage and Algorithm
Explained](https://CRAN.R-project.org/package=dedupewider/vignettes/usage-and-algorithm-explained.html)
