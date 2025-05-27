
<!-- README.md is generated from README.Rmd. Please edit that file -->

# calendario

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/calendario)](https://CRAN.R-project.org/package=calendario)
<!-- badges: end -->

Personal package to help me track my tasks. Unlikely to be of value to
anyone else.

## Installation

You can install the development version of calendario like so:

``` r
pak::pak("djnavarro/calendario")
```

## Example

``` r
library(calendario)
cal <- calendario$new()

# verbose way to do it that I never do
cal$add_task(
  description = "it's a thing",
  start = as.Date("2025-10-11"),
  stop = as.Date("2025-10-14"),
  project = "be gay",
  team  = "danielle"
)

# more convenient
cal$set_project("do crime")
cal$add_task("crime 1", "12 oct")
cal$add_task("crime 2", "12 oct", "14 oct")

# most common workflow
cal |>
  project("be happy") |>
  task("thing 1") |>
  task("thing 2")
#> <calendario object>
#> • be gay [1 task]
#> • be happy [2 tasks]
#> • do crime [2 tasks]

# printing gives minimal summary
cal 
#> <calendario object>
#> • be gay [1 task]
#> • be happy [2 tasks]
#> • do crime [2 tasks]

# extracting tasks returns a tibble
cal$get_tasks()
#> # A tibble: 5 × 7
#>   project  type  description  start      stop       hours team    
#>   <chr>    <chr> <chr>        <date>     <date>     <dbl> <chr>   
#> 1 be gay   work  it's a thing 2025-10-11 2025-10-14     2 danielle
#> 2 do crime work  crime 1      2024-10-12 2024-10-12     2 danielle
#> 3 do crime work  crime 2      2024-10-12 2024-10-14     2 danielle
#> 4 be happy work  thing 1      2025-05-27 2025-05-27     2 danielle
#> 5 be happy work  thing 2      2025-05-27 2025-05-27     2 danielle
```
