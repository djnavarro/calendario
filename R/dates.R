
# separators allowed in lazy date codes
lazy_date_sep <- function() {
  c(" ", "/", "-")
}

# regex that matches a lazy date separator
lazy_date_sep_regex <- function() {
  paste0("[", paste(lazy_date_sep(), collapse = ""), "]")
}

# lazy dates are "10 may", "1 jun", etc; assumed to be the same 
# year as today, unless that would lead to a date that is in the
# past. there is a "tolerance" allowed before an apparently past 
# date is rolled forward to next year, set to 56 days by default
parse_lazy_date <- function(x, tol = 56, base_year = NULL) {
  
  # convert lazy day/month strings to canonical form
  x <- purrr::map_chr(
    strsplit(x, split = lazy_date_sep_regex()), 
    \(x) {l <- nchar(x); if(l[2] > l[1]) return(paste(x[1], x[2])); paste(x[2], x[1])}
  )

  # supply a default year if none provided
  if (is.null(base_year)) base_year <- lubridate::year(lubridate::today())
  
  # parse dates on the logic that the date refers to the current year
  parsed_date <- lubridate::dmy(paste(x, base_year))

  # dates that look "too far" in the past (as judged by the tolerance) are 
  # rolled forward to next year
  diff <- as.numeric(difftime(parsed_date, lubridate::today(), units = "days"))
  is_next_year <- diff <= -tol
  if (any(is_next_year)) {
    parsed_date[is_next_year] <- lubridate::dmy(paste(x[is_next_year], base_year + 1))
  }
  
  return(parsed_date)
}

# vector of date objects
date_vec <- function(start, stop, parse_date = NULL) {
  if (is.null(parse_date)) parse_date <- lubridate::ymd
  if (!lubridate::is.Date(start)) start <- parse_date(start)
  if (!lubridate::is.Date(stop)) stop <- parse_date(stop)
  as.Date(start:stop)
}


#friday <- function() {
#  weekday <- lubridate::wday(lubridate::today(), week_start = 6)
#  lubridate::today() + 7 - weekday
#}
