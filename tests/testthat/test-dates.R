
test_that("date_vec returns date vectors", {

  # check no error
  expect_no_error(date_vec("2020-01-01", "2020-12-31"))
  expect_no_error(date_vec("2021-01-01", "2021-12-31"))

  # test objects
  year_2020 <- date_vec("2020-01-01", "2020-12-31")
  year_2021 <- date_vec("2021-01-01", "2021-12-31")

  # check class 
  expect_s3_class(year_2020, "Date")
  expect_s3_class(year_2021, "Date")

  # handles leap years
  expect_length(year_2020, 366L)
  expect_length(year_2021, 365L)

})

test_that("parse_lazy_date handles day/month codes", {

    # test dates for this test are always in the past, and cover a full leap year
    true_dates <- date_vec("2020-01-01", "2020-12-31")
  
    # acceptable day strings can use leading zeros or not
    day_num <- lubridate::day(true_dates)
    day_str <- list(
      d  = as.character(day_num),                           # 1, 2
      dd = gsub("^([0-9])$", "0\\1", as.character(day_num)) # 01, 02
    )
  
    # acceptable month letter strings are English three letter codes
    mon_num <- lubridate::month(true_dates)
    mon_str <- list(
      Mmm = month.abb[mon_num],          # Jan, Feb
      mmm = tolower(month.abb[mon_num]), # jan, feb
      MMM = toupper(month.abb[mon_num])  # JAN, FEB
    )
  
    # all combinations of the above should work
    for(i in seq_along(day_str)) {
      for(j in seq_along(mon_str)) {
        for(s in lazy_date_sep()) {

          # dm order
          lazy_dates <- paste(day_str[[i]], mon_str[[j]], sep = s)
          expect_equal(
            object = parse_lazy_date(lazy_dates, year_start = lubridate::ymd("2020 01 01")),
            expected = true_dates
          )

          # md order
          lazy_dates <- paste(day_str[[i]], mon_str[[j]], sep = s)
          expect_equal(
            object = parse_lazy_date(lazy_dates, year_start = lubridate::ymd("2020 01 01")),
            expected = true_dates
          )

        }
      }
    }  

})

test_that("parse_lazy_date rollover works", {

  today <- lubridate::today()
  y <- lubridate::year(today)
  m <- lubridate::month(today)
  d <- lubridate::day(today)

  true_dates <- date_vec(
    start = lubridate::ymd(paste(y, "1", "1")), 
    stop = lubridate::ymd(paste(y, "12", "31"))
  )

  lazy_dates <- paste(
    as.character(lubridate::day(true_dates)), 
    month.abb[lubridate::month(true_dates)]
  )

  diffs <- as.numeric(difftime(true_dates, today, units = "days"))

  for(tol in range(diffs)) {

    expected_rollover <- diffs < -tol 
    parsed_lazy_dates <- parse_lazy_date(lazy_dates, year_start = lubridate::today() - tol)
    actual_rollover <- lubridate::year(parsed_lazy_dates) == y + 1
    expect_equal(actual_rollover, expected_rollover)

  } 

})

test_that("parse_lazy_date NA behaviour", {

  # return NA if either the day or the month is missing
  test <- c("12", "apr", "12 cat", "cat apr")
  expect_equal(
    object = parse_lazy_date(test),
    expected = as.Date(rep(NA, length(test)))
  )

  # returns NA if the day or month are multiply matched
  test <- c("12 12 apr", "12 apr 12", "12 apr apr", "12 apr may", "may 12 may", "12 12", "may jun")
  expect_equal(
    object = parse_lazy_date(test),
    expected = as.Date(rep(NA, length(test)))
  )

  # BUT! will return dates if there are non-matched strings or no separators
  # TODO: decide if this is desirable behaviour!
  test <- c("tue 12 apr", "wed 12 apr", "cat apr 12", "apr12", "12apr","12apricot")
  expect_equal(
    object = parse_lazy_date(test, year_start = lubridate::ymd("2020 01 01")),
    expected = rep(lubridate::ymd("2020 04 12"), length(test))
  )

})