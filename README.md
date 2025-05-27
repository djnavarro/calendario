
<!-- README.md is generated from README.Rmd. Please edit that file -->

# calendario

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/calendario)](https://CRAN.R-project.org/package=calendario)
<!-- badges: end -->

This is a personal package I use to help me keep track of my tasks. It
is unlikely to be of value to anyone else

## Installation

You can install the development version of calendario like so:

``` r
pak::pak("djnavarro/calendario")
```

## Example

``` r
library(calendario)
tasks <- calendario$new()

# verbose way to do it that I never do
tasks$add_task(
  description = "it's a thing",
  start = as.Date("2025-10-11"),
  stop = as.Date("2025-10-14"),
  project = "be gay",
  team  = "danielle"
)

# more convenient
tasks$set_project("do crime")
tasks$add_task("crime 1", "12 oct")
tasks$add_task("crime 2", "12 oct", "14 oct")

# print
tasks
#> <calendario object>
#> • be gay [1 task]
#> • do crime [2 tasks]
tasks$get_tasks()
#> # A tibble: 3 × 7
#>   project  type  description  start      stop       hours team    
#>   <chr>    <chr> <chr>        <date>     <date>     <dbl> <chr>   
#> 1 be gay   work  it's a thing 2025-10-11 2025-10-14     2 danielle
#> 2 do crime work  crime 1      2024-10-12 2024-10-12     2 danielle
#> 3 do crime work  crime 2      2024-10-12 2024-10-14     2 danielle
```
