
# old code

parse_lazy_date <- function(when, threshold = 9) {
  month <- gsub("[0123456789 ]", "", when)
  year <- "2024"
  if (grep(month, tolower(month.abb)) < threshold) {
    year <- "2025"
  }
  str <- paste(when, year)
  lubridate::dmy(str)
}


# weekday functions

#friday <- function() {
#  weekday <- lubridate::wday(lubridate::today(), week_start = 6)
#  lubridate::today() + 7 - weekday
#}
