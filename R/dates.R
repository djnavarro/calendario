
# separators allowed in lazy date codes
lazy_date_sep <- function() {
  c(" ", "/", "-")
}

# regex that matches a lazy date separator
lazy_date_sep_regex <- function() {
  paste0("[", paste(lazy_date_sep(), collapse = ""), "]")
}

# convert an informal day month string to a tibble of numeric days and months; it 
# requires months to be specified with three letter codes, and days to be numeric.
# it returns NA for the relevant field if there are multiple matches or no matches.
# it does not check if the combination is valid (e.g., "31 feb" will return day 31
# month 2)
parse_dm <- function(x) {

  # acceptable patterns for days and months
  month_regex <- paste("(", tolower(month.abb), ")", collapse = "|", sep = "")
  day_regex <- "[0-9]+"
  x <- tolower(x)

  # extract all matches
  month <- stringr::str_extract_all(x, month_regex)
  day <- stringr::str_extract_all(x, day_regex)

  # but treat as NA if it is not exactly one match
  month <- purrr::map_chr(month, \(m) ifelse(length(m) == 1, m, NA_character_))
  day <- purrr::map_chr(day, \(d) ifelse(length(d) == 1, d, NA_character_))

  # convert to numeric
  month_num <- 1:12
  names(month_num) <- tolower(month.abb)
  month <- unname(month_num[month])
  day <- as.numeric(day)
  day[day > 31] <- NA

  return(tibble::tibble(day = day, month = month))

}



# lazy dates are "10 may", "1 jun", etc; assumed to be the same 
# year as today, unless that would lead to a date that is in the
# past. there is a "tolerance" allowed before an apparently past 
# date is rolled forward to next year, set to 56 days by default
parse_lazy_date <- function(x, year_start = lubridate::today() - 90) {
  
  
  dmy_start <- c(
    day = lubridate::day(year_start),
    month = lubridate::month(year_start),
    year = lubridate::year(year_start)
  )

  dmy <- parse_dm(x) |>
    dplyr::mutate(year = dplyr::case_when(
      month > dmy_start["month"] ~ dmy_start["year"],
      month == dmy_start["month"] & day >= dmy_start["day"] ~ dmy_start["year"],
      TRUE ~ dmy_start["year"] + 1
    ))

  parsed_date <- as.Date(unlist(purrr::pmap(dmy, lubridate::make_date)))  
  return(parsed_date)
}

# vector of date objects
date_vec <- function(start, stop, parse_date = NULL) {
  if (is.null(parse_date)) parse_date <- lubridate::ymd
  if (!lubridate::is.Date(start)) start <- parse_date(start)
  if (!lubridate::is.Date(stop)) stop <- parse_date(stop)
  as.Date(start:stop)
}

# check if x is a weekday
is_weekday <- function(x) {
  d <-  lubridate::wday(x, label = TRUE)
  !(d %in% c("Sat", "Sun"))
}

# count the number of weekdays TODO: this is inefficient
n_weekdays <- function(start, stop) {
  days <- date_vec(start, stop)
  sum(is_weekday(days))
}

# add the weekdays TODO: this is awful
add_weekdays <- function(x, n) {
  if (n == 0) return(x)
  if (n > 0) {
    while (n > 0) {
      x <- x + 1
      if (is_weekday(x)) n <- n - 1
    }
    return(x)
  }
  while (n < 0) {
    x <- x - 1
    if (is_weekday(x)) n <- n + 1
  }
  return(x)
}

#friday <- function() {
#  weekday <- lubridate::wday(lubridate::today(), week_start = 6)
#  lubridate::today() + 7 - weekday
#}
