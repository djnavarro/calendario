#' R6 Class Representing Tasks and Projects
#'
#' @description
#' A calendario object tracks a collection of projects that are contain 
#' tasks that are to be performed at different times 
#'
#' @details
#' BLAH BLAH BLAH
#' 
#' @export
Calendario <- R6::R6Class(
  classname = "Calendario",
  public = list(

    #' @description
    #' Retrieve a table of stored tasks
    #' 
    #' @param ... Arguments passed to dplyr::filter()
    #' 
    #' @return A tibble with one row per task
    #' 
    #' @examples
    #' cal <- Calendario$new()
    #' cal$set_project("my project")
    #' cal$add_task("first task", "jan 21")
    #' cal$add_task("second task", "jan 22")
    #' cal$get_tasks()
    #' 
    get_tasks = function(...) { 
      dplyr::filter(private$tasks, ...)
    },

    #' @description
    #' Set the default project (special case of `set_default()`)
    #' 
    #' @param value Name of the new default project
    #' 
    #' @examples
    #' cal <- Calendario$new()
    #' cal$set_project("my project")
    #' cal$get_project()
    #' 
    set_project = function(value) {private$default$project <- value},

    #' @description
    #' Set the default value for one or more specified task fields
    #' 
    #' @param ... Name-value pairs specifying task fields and new default values
    #' 
    #' @examples
    #' cal <- Calendario$new()
    #' cal$set_defaults(type = "fun", team = "danielle")
    #' cal$get_defaults()
    #' 
    set_defaults = function(...) {
      pairs <- rlang::list2(...)
      names <- names(pairs)
      purrr::walk2(names, pairs, \(n, v) private$default[[n]] <- v)
    },

    #' @description
    #' Retrieve the name of the current default project
    #' 
    #' @return A character vector of length 1
    #' 
    #' @examples
    #' cal <- Calendario$new()
    #' cal$set_project("my project")
    #' cal$get_project()
    #
    get_project = function() {private$default$project},

    #' @description
    #' Retrieve the current defaults for all task fields
    #' 
    #' @return A tibble with one row
    #' 
    #' @examples
    #' cal <- Calendario$new()
    #' cal$get_defaults()
    #
    get_defaults = function() {private$default},

    #' @description
    #' Retrieve the options list
    #' 
    #' @return A list
    #' 
    #' @examples
    #' cal <- Calendario$new()
    #' cal$get_options()
    #
    get_options = function() {private$options},


    #' @description
    #' Add a task to a project
    #' 
    #' @param description Character string providing a description of the task
    #' @param start Date the work starts (defaults to current date)
    #' @param stop Date the work stops (defaults to same day as start)
    #' @param daily_hours Number of hours per day the task takes
    #' @param total_hours Number of hours in total the task takes
    #' @param type Character string assigning the task to a category
    #' @param project Character string naming the project the task falls within
    #' @param team Character string describing the team
    #' 
    #' @examples
    #' cal <- Calendario$new()
    #' cal$add_task(
    #'   description = "label for the task",
    #'   start = as.Date("2025-01-24"),
    #'   stop = as.Date("2025-01-26"),
    #'   daily_hours = 1,
    #'   project = "my project",
    #'   team = "just me"
    #' )
    #' cal$get_tasks()
    #
    add_task = function(description = NULL,
                        start = NULL,
                        stop = NULL,
                        daily_hours = NULL,
                        total_hours = NULL,
                        type = NULL,
                        project = NULL,
                        team = NULL) {

      # apply simple defaults
      if (is.null(description)) description <- private$default$description
      if (is.null(type)) type <- private$default$type
      if (is.null(project)) project <- private$default$project
      if (is.null(team)) team <- private$default$team

      # use helper functions to supply default task dates
      if (is.null(start)) start <- private$options$date_task_start()
      if (is.null(stop)) stop <- private$options$date_task_stop(start)
      
      # handle lazy date strings
      if (!inherits(start, "Date")) start <- parse_lazy_date(start)
      if (!inherits(stop, "Date")) stop <- parse_lazy_date(stop)  
      
      # compute the task date in terms of weekdays
      days <- n_weekdays(start, stop)

      # fill out daily and total hours
      if (is.null(daily_hours) & is.null(total_hours)) {
        daily_hours <- private$default$daily_hours
        total_hours <- daily_hours * days
      } 
      if (is.null(daily_hours) & !is.null(total_hours)) {
        daily_hours <- total_hours / days
      }
      if (!is.null(daily_hours) & is.null(total_hours)) {
        total_hours <- daily_hours * days
      }
      if (!is.null(daily_hours) & !is.null(total_hours)) {
        total_hours <- daily_hours * days       
      }

      # store the task
      private$tasks <- private$tasks |>
        tibble::add_row(
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
    }, 

    #' @description
    #' Show a table of stored tasks as a flextable
    #' 
    #' @param ... Arguments passed to dplyr::filter()
    #' 
    #' @return A flextable object
    #' 
    #' @examples
    #' cal <- Calendario$new()
    #' cal$set_project("my project")
    #' cal$add_task("first task", "jan 21")
    #' cal$add_task("second task", "jan 23")
    #' cal$show_tasks()
    #' 
    show_tasks = function(...) {

      data <- private$tasks |> 
        dplyr::filter(...)

      if(nrow(data) == 0) {
        cat(":)\n")
        return(invisible(NULL))
      }
  
      do.call(
        what = flextable::set_flextable_defaults,
        args = private$options$flextable_options
      )

      data |>
        dplyr::arrange(start) |>
        flextable::flextable() |> 
        flextable::set_header_labels(
          project = "Project", 
          type = "Type", 
          description = "Description", 
          start = "Start date",
          stop = "End date",
          days = "Work days",
          daily_hours = "Daily hours",
          total_hours = "Total hours",
          team = "Team"  
        ) |> 
        flextable::autofit()
    },

    #' @description
    #' Retrieve a table describing daily workload
    #' 
    #' @param start Date the tabulation starts (defaults to current date)
    #' @param stop Date the tabulation stops (defaults to start date plus 90 days)
    #' 
    #' @return A tibble with one row per day
    #' 
    #' @examples
    #' cal <- Calendario$new()
    #' cal$set_project("my project")
    #' cal$add_task("first task", "jan 21")
    #' cal$add_task("second task", "jan 23")
    #' cal$get_workload()
    #' 
    get_workload = function(start = NULL, stop = NULL) {
                              
      if (is.null(start)) start <- private$options$date_range_start()      
      if (is.null(stop)) stop <- private$options$date_range_stop(start)                    
      if(!inherits(start, "Date")) start <- parse_lazy_date(start)
      if(!inherits(stop, "Date")) stop <- parse_lazy_date(stop)
                          
      base <- tibble::tibble(
        date = date_vec(start, stop), 
        daily_hours = 0
      )
    
      private$tasks |>
        purrr::pmap(
          \(..., start, stop, daily_hours) {
            tibble::tibble(
              date = date_vec(start, stop), 
              daily_hours = daily_hours
            )
          }
        ) |>
        dplyr::bind_rows() |>
        dplyr::bind_rows(base) |>
        dplyr::summarise(daily_hours = sum(daily_hours), .by = date) |>
        dplyr::arrange(date) |>
        dplyr::mutate(
          weekday  = lubridate::wday(date, label = TRUE),
          month    = lubridate::month(date, label = TRUE),
          monthday = lubridate::mday(date),
          week     = cumsum(weekday == "Mon")
        ) |>
        dplyr::filter(!(weekday %in% c("Sat", "Sun"))) # TODO: support weekends eventually?
    },

    #' @description
    #' Retrieve a list of tables describing monthly workload
    #' 
    #' @param start Date the tabulation starts (defaults to current date)
    #' @param stop Date the tabulation stops (defaults to start date plus 90 days)
    #' 
    #' @return A tibble with one row per month
    #' 
    #' @examples
    #' cal <- Calendario$new()
    #' cal$set_project("my project")
    #' cal$add_task("first task", "jan 21")
    #' cal$add_task("second task", "jan 23")
    #' cal$get_workload(start = "jan 1", stop = "feb 16")
    #'     
    get_calendar = function(start = NULL, stop = NULL) {

      if (is.null(start)) start <- private$options$date_range_start()      
      if (is.null(stop)) stop <- private$options$date_range_stop(start)
      
      work <- self$get_workload(start = start, stop = stop)

      span_str <- function(x) {
        lo <- min(x)
        hi <- max(x)
        if (lo == hi) return(as.character(lo))
        paste(lo, hi, sep = "-")
      }

      work_month <- function(data) {
        data |>
          dplyr::group_by(week) |>
          dplyr::mutate(
            dayspan = span_str(monthday),
            weektotal = sum(daily_hours), 
          ) |>
          dplyr::ungroup() |>
          dplyr::select(weekday, month, daily_hours, dayspan, weektotal) |>
          tidyr::pivot_wider(names_from = weekday, values_from = daily_hours) |> 
          dplyr::select(
            Month = month, 
            Days = dayspan, 
            Mon, Tue, Wed, Thu, Fri, 
            Total = weektotal
          )
      }

      mcal <- work |> 
        dplyr::group_by(month) |> 
        dplyr::group_split() |> 
        purrr::map(work_month)
      
      mcal
    },

    #' @description
    #' Show monthly workload as a flextable
    #' 
    #' @param start Date the tabulation starts (defaults to current date)
    #' @param stop Date the tabulation stops (defaults to start date plus 90 days)
    #' 
    #' @return A flextable object
    #' 
    #' @examples
    #' cal <- Calendario$new()
    #' cal$set_project("my project")
    #' cal$add_task("first task", "jan 21")
    #' cal$add_task("second task", "jan 23")
    #' cal$show_calendar(start = "jan 1", stop = "feb 16")
    #'     
    show_calendar = function(start = NULL, stop = NULL) {

      if (is.null(start)) start <- private$options$date_range_start()      
      if (is.null(stop)) stop <- private$options$date_range_stop(start)
  
      do.call(
        what = flextable::set_flextable_defaults,
        args = private$options$flextable_options
      )

      mcal <- self$get_calendar(start = start, stop = stop)
      mcal |> 
        dplyr::bind_rows() |>
        flextable::as_grouped_data("Month") |> 
        flextable::flextable(
          cwidth = c(.75, .75, .5, .5, .5, .5, .5, .75)
        )
    
    }
  ),

  private = list(

    tasks = tibble::tibble(
      project = character(),
      type = character(),
      description = character(),
      start = as.Date(character(0L)),
      stop = as.Date(character(0L)),
      days = numeric(),
      daily_hours = numeric(),
      total_hours = numeric(),
      team = character()
    ),
    
    default = tibble::tibble(
      project = "Project",
      type = NA_character_,
      description = "Description",
      start = as.Date(NA),
      stop = as.Date(NA),
      days = NA_real_,
      daily_hours = 1,
      total_hours = 1,
      team = NA_character_
    ),

    options = list(
      flextable_options = list(
        theme_fun = flextable::theme_alafoli,
        font.size = 8,
        fmt_date = "%a %b %d %Y",
        background.color = "#ffffff"
      ),
      date_range_start = function() lubridate::today(),
      date_range_stop = function(start = NULL, span = 90) {
        if(is.null(start)) return(lubridate::today() + span)
        start
      },
      date_task_start = function() lubridate::today(),
      date_task_stop = function(start = NULL) {
        if(is.null(start)) return(lubridate::today())
        start
      }

    )


  )
)

#' @exportS3Method
print.Calendario <- function(x, ...) {
  n_task <- table(x$get_tasks()$project)
  project_name <- names(n_task)
  n_proj <- length(project_name)
  if (n_proj == 0) {
    cli::cli_text("<Calendario object [0 projects]>")
  
  } else if (n_proj == 1) {
    project_info <- paste0("[", n_task, " task", ifelse(n_task == 1, "]", "s]"))
    cli::cli_text("<Calendario object [1 project]>")
    cli::cli_ul(paste(project_name, project_info))

  } else {
    project_info <- paste0("[", n_task, " task", ifelse(n_task == 1, "]", "s]"))
    cli::cli_text(paste0("<Calendario object [", n_proj, " projects]>"))
    cli::cli_ul(paste(project_name, project_info))

  }
  return(invisible(x))
}
