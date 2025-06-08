
# provides task objects

`%||%` <- purrr::`%||%`

validate_length <- function(object, length) {
  obj_name <- rlang::as_name(rlang::enquo(object))
  if (length(object) == length) return(invisible(NULL))
  rlang::abort(paste0("`", obj_name, "` must be length ", length))
}

validate_class <- function(object, class) {
  obj_name <- rlang::as_name(rlang::enquo(object))
  if (inherits(object, class)) return(invisible(NULL))
  rlang::abort(paste0("`", obj_name, "` must be a ", class, " object"))
}

validate_numeric <- function(object) {
  obj_name <- rlang::as_name(rlang::enquo(object))
  if (is.numeric(object)) return(invisible(NULL))
  rlang::abort(paste0("`", obj_name, "` must be numeric"))
}

# does not supply default values, but will enforce classes, lengths, and
# consistency whenever it can
new_task <- function(project = NULL,
                     type = NULL,
                     description = NULL,
                     start = NULL,
                     stop = NULL,
                     days = NULL,
                     daily_hours = NULL,
                     total_hours = NULL,
                     team = NULL
                    ) {

  # replace NULLs with missings of the appropriate type
  project     <- project %||% NA_character_
  type        <- type %||% NA_character_
  description <- description %||% NA_character_
  start       <- start %||% as.Date(NA_real_)
  stop        <- stop %||% as.Date(NA_real_)
  days        <- days %||% NA_integer_
  daily_hours <- daily_hours %||% NA_real_
  total_hours <- total_hours %||% NA_real_
  team        <- team %||% NA_character_

  # enforce length
  validate_length(project, 1)
  validate_length(type, 1)
  validate_length(description, 1)
  validate_length(start, 1)
  validate_length(stop, 1)
  validate_length(days, 1)
  validate_length(daily_hours, 1)
  validate_length(total_hours, 1)
  validate_length(team, 1)

  # coerce string fields to character
  if (!inherits(project, "character")) project <- as.character(project)
  if (!inherits(type, "character")) type <- as.character(type)
  if (!inherits(description, "character")) description <- as.character(description)
  if (!inherits(team, "character")) team <- as.character(team)

  # check classes
  validate_class(start, "Date")
  validate_class(stop, "Date")
  validate_numeric(days)
  validate_numeric(daily_hours)
  validate_numeric(total_hours)

  if (!is.numeric(days)) rlang::abort("`days` must be numeric")
  
  # only two of "start", "stop", and "days" are required to populate the third 
  # NOTE: adding and subtracting work days can be intransitive if a pivot date 
  # is a weekend.
  if (!is.na(start) & !is.na(stop) & is.na(days)) days <- n_weekdays(start, stop)
  if (!is.na(start) & is.na(stop) & !is.na(days)) stop <- add_weekdays(start, days)
  if (is.na(start) & !is.na(stop) & !is.na(days)) start <- add_weekdays(stop, -days)

  # if "daily_hours" and "total_hours" are supplied, require consistency with "days"
  if (!is.na(daily_hours) & !is.na(total_hours)) {
    if (abs(daily_hours * days - total_hours) > 1) { # allow a little wiggle room
      rlang::abort("if `daily_hours` and `total_hours` are supplied, they must be consistent with dates")
    }
  }

  # if only one is supplied, populate using the other and "days"
  if (is.na(daily_hours)) daily_hours <- total_hours / max(days, 1)
  if (is.na(total_hours)) total_hours <- daily_hours * max(days, 1)

  # place into a tibble
  task <- tibble::tibble(
    project = project,
    type = type,
    description = description,
    start = start,
    stop = stop,
    days = days,
    daily_hours = daily_hours,
    total_hours = total_hours,
    team = team
  )

  # add additional class and return
  class(task) <- c("cal_task", class(task))
  return(task)                  
}

empty_task <- function() new_task()[-1,]

#print.cal_task <- function(x, ...) {
#  if (nrow(x) == 0) {
#    cli::cli_text("<empty task>")
#  } else {
#    class(x) <- class(x)[-1]
#    print(x, ...)
#  }
#  return(invisible(x))
#}

