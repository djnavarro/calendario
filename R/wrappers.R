
#' @title
#' Calendario Wrapper Functions 
#'  
#' @description
#' `cal_add_task()`, `cal_set_project()`, and the other functions here 
#' are convenience functions that allow a piped workflow with calendario 
#' objects
#'
#' @details
#' These functions are wrappers around one of the calendario object
#' methods: `cal_add_task()` is a wrapper around `$add_task()`, 
#' `cal_set_project()` is a wrapper around `$set_project()` and so
#' on. In all cases the functions take a calendario object `x` as 
#' their first argument, and all other arguments are passed via `...` 
#' to the relevant calendario R6 method. The one exception to this 
#' rule is `cal_new()`, for which `...` is the only argument.
#' 
#' @param x A calendario object
#' @param ... Arguments to be passed to the relevant R6 method
#' 
#' @return The calendario object. For all functions except `cal_new()`
#' the object is returned invisibly
#' 
#' @name wrappers
#' 
#' @examples
#' cal <- cal_new()
#' cal |>
#'   cal_set_project("project-name") |>
#'   cal_add_task("description-1", "29 may") |>
#'   cal_add_task("description-2", "1 jun")
#' 
#' cal_get_workload(cal)
#' cal_get_tasks(cal)
NULL

#' @rdname wrappers
#' @export
cal_new <- function(...) {
  Calendario$new(...)
}

#' @rdname wrappers
#' @export
cal_set_project <- function(x, ...) {
  x$set_project(...)
  invisible(x)
}

#' @rdname wrappers
#' @export
cal_set_default <- function(x, ...) {
  x$set_default(...)
  invisible(x)
}

#' @rdname wrappers
#' @export
cal_add_task <- function(x, ...) {
  x$add_task(...)
  invisible(x)
}

#' @rdname wrappers
#' @export
cal_get_tasks <- function(x, ...) {
  x$get_tasks(...)
  invisible(x)
}

#' @rdname wrappers
#' @export
cal_get_workload <- function(x, ...) {
  x$get_workload(...)
  invisible(x)
}

#' @rdname wrappers
#' @export
cal_get_calendar <- function(x, ...) {
  x$get_calendar(...)
  invisible(x)
}
