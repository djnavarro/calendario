
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
#> 2 do crime work  crime 1      2026-05-12 2026-05-12     2 danielle
#> 3 do crime work  crime 2      2026-05-12 2025-05-14     2 danielle
#> 4 be happy work  thing 1      2025-06-02 2025-06-02     2 danielle
#> 5 be happy work  thing 2      2025-06-13 2025-06-13     2 danielle

# extracting a calendar returns a list of tibbles
cal$get_calendar("1 may", "11 jul")
#> [[1]]
#> # A tibble: 5 × 6
#>   Week        Mon   Tue   Wed   Thu   Fri
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Jan 1-31     NA    NA    NA     2     2
#> 2 Jan 5-9       2     2     2     2     2
#> 3 Jan 12-16     2     2     2     2     2
#> 4 Jan 19-23     2     2     2     2     2
#> 5 Jan 26-30     2     2     2     2     2
#> 
#> [[2]]
#> # A tibble: 4 × 6
#>   Week        Mon   Tue   Wed   Thu   Fri
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Feb 2-6       2     2     2     2     2
#> 2 Feb 9-13      2     2     2     2     2
#> 3 Feb 16-20     2     2     2     2     2
#> 4 Feb 23-27     2     2     2     2     2
#> 
#> [[3]]
#> # A tibble: 5 × 6
#>   Week        Mon   Tue   Wed   Thu   Fri
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Mar 2-6       2     2     2     2     2
#> 2 Mar 9-13      2     2     2     2     2
#> 3 Mar 16-20     2     2     2     2     2
#> 4 Mar 23-27     2     2     2     2     2
#> 5 Mar 1-31      2     2    NA    NA    NA
#> 
#> [[4]]
#> # A tibble: 5 × 6
#>   Week        Mon   Tue   Wed   Thu   Fri
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Apr 1-31     NA    NA     2     2     2
#> 2 Apr 6-10      2     2     2     2     2
#> 3 Apr 13-17     2     2     2     2     2
#> 4 Apr 20-24     2     2     2     2     2
#> 5 Apr 1-30      2     2     2     2    NA
#> 
#> [[5]]
#> # A tibble: 6 × 6
#>   Week        Mon   Tue   Wed   Thu   Fri
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 May 12-16     2     2     4     2     2
#> 2 May 19-23     2     2     2     2     2
#> 3 May 26-30     2     2     2     2     2
#> 4 May 1-30     NA    NA    NA    NA     2
#> 5 May 4-8       2     2     2     2     2
#> 6 May 11-12     2     4    NA    NA    NA
#> 
#> [[6]]
#> # A tibble: 5 × 6
#>   Week        Mon   Tue   Wed   Thu   Fri
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Jun 2-6       4     2     2     2     2
#> 2 Jun 9-13      2     2     2     2     4
#> 3 Jun 16-20     2     2     2     2     2
#> 4 Jun 23-27     2     2     2     2     2
#> 5 Jun 1-30      2    NA    NA    NA    NA
#> 
#> [[7]]
#> # A tibble: 5 × 6
#>   Week        Mon   Tue   Wed   Thu   Fri
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Jul 1-30     NA     2     2     2     2
#> 2 Jul 7-11      2     2     2     2     2
#> 3 Jul 14-18     2     2     2     2     2
#> 4 Jul 21-25     2     2     2     2     2
#> 5 Jul 1-31      2     2     2     2    NA
#> 
#> [[8]]
#> # A tibble: 5 × 6
#>   Week        Mon   Tue   Wed   Thu   Fri
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Aug 1-31     NA    NA    NA    NA     2
#> 2 Aug 4-8       2     2     2     2     2
#> 3 Aug 11-15     2     2     2     2     2
#> 4 Aug 18-22     2     2     2     2     2
#> 5 Aug 25-29     2     2     2     2     2
#> 
#> [[9]]
#> # A tibble: 5 × 6
#>   Week        Mon   Tue   Wed   Thu   Fri
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Sep 1-5       2     2     2     2     2
#> 2 Sep 8-12      2     2     2     2     2
#> 3 Sep 15-19     2     2     2     2     2
#> 4 Sep 22-26     2     2     2     2     2
#> 5 Sep 1-30      2     2    NA    NA    NA
#> 
#> [[10]]
#> # A tibble: 5 × 6
#>   Week        Mon   Tue   Wed   Thu   Fri
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Oct 1-30     NA    NA     2     2     2
#> 2 Oct 6-10      2     2     2     2     2
#> 3 Oct 13-17     2     2     2     2     2
#> 4 Oct 20-24     2     2     2     2     2
#> 5 Oct 27-31     2     2     2     2     2
#> 
#> [[11]]
#> # A tibble: 4 × 6
#>   Week        Mon   Tue   Wed   Thu   Fri
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Nov 3-7       2     2     2     2     2
#> 2 Nov 10-14     2     2     2     2     2
#> 3 Nov 17-21     2     2     2     2     2
#> 4 Nov 24-28     2     2     2     2     2
#> 
#> [[12]]
#> # A tibble: 5 × 6
#>   Week        Mon   Tue   Wed   Thu   Fri
#>   <chr>     <dbl> <dbl> <dbl> <dbl> <dbl>
#> 1 Dec 1-5       2     2     2     2     2
#> 2 Dec 8-12      2     2     2     2     2
#> 3 Dec 15-19     2     2     2     2     2
#> 4 Dec 22-26     2     2     2     2     2
#> 5 Dec 1-31      2     2     2    NA    NA
```
