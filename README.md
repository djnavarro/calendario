
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
  start = as.Date("2025-05-11"),
  stop = as.Date("2025-05-14"),
  project = "be gay",
  team  = "danielle"
)

# more convenient
cal$set_project("do crime")
cal$add_task("crime 1", "12 may")
cal$add_task("crime 2", "12 may", "14 may")

# most common workflow
cal |>
  project("be happy") |>
  task("thing 1", "2 jun") |>
  task("thing 2", "13 jun")
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
#> 1 be gay   work  it's a thing 2025-05-11 2025-05-14     2 danielle
#> 2 do crime work  crime 1      2025-05-12 2025-05-12     2 danielle
#> 3 do crime work  crime 2      2025-05-12 2025-05-14     2 danielle
#> 4 be happy work  thing 1      2025-06-02 2025-06-02     2 danielle
#> 5 be happy work  thing 2      2025-06-13 2025-06-13     2 danielle

# extracting a calendar returns a list of tibbles
cal$get_calendar("1 may", "11 jul")
#> [[1]]
#> # A tibble: 5 × 6
#>   Week        Mon   Tue   Wed   Thu   Fri
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 May 1-2      NA    NA    NA     0     0
#> 2 May 5-9       0     0     0     0     0
#> 3 May 12-16     6     4     4     0     0
#> 4 May 19-23     0     0     0     0     0
#> 5 May 26-30     0     0     0     0     0
#> 
#> [[2]]
#> # A tibble: 5 × 6
#>   Week        Mon   Tue   Wed   Thu   Fri
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Jun 2-6       2     0     0     0     0
#> 2 Jun 9-13      0     0     0     0     2
#> 3 Jun 16-20     0     0     0     0     0
#> 4 Jun 23-27     0     0     0     0     0
#> 5 Jun 1-30      0    NA    NA    NA    NA
#> 
#> [[3]]
#> # A tibble: 2 × 6
#>   Week       Mon   Tue   Wed   Thu   Fri
#>   <chr>    <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Jul 1-30    NA     0     0     0     0
#> 2 Jul 7-11     0     0     0     0     0
```
