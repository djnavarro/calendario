#' @export
calendario <- R6::R6Class(
  classname = "calendario",
  public = list(

    get_tasks = function() {private$tasks},

    set_project = function(project) {private$project <- project},

    add_task = function(description = NULL, # string describing the work (defaults to "work")
                        start = NULL,       # date the work starts (defaults to current date)
                        stop = NULL,        # date the work stops (defaults to start)
                        hours = NULL,       # average hours per day (defaults to 2 hours/day)
                        type = NULL,        # a category label (defaults to "work")
                        project = NULL,     # project name (defaults to "project")
                        team = NULL) {      # team (defaults to "danielle")

      if(is.null(project)) project <- private$project
      if(is.null(type)) type <- "work"
      if(is.null(description)) description <- "work"
      if(is.null(start)) start <- lubridate::today()
      if(is.null(stop)) stop <- start
      if(is.null(team)) team <- "danielle"
      if(is.null(hours)) hours <- 2

      if(!inherits(start, "Date")) start <- parse_lazy_date(start)
      if(!inherits(stop, "Date")) stop <- parse_lazy_date(stop)

      private$tasks <- private$tasks |>
        tibble::add_row(
          project = project,
          type = type,
          description = description,
          start = start,
          stop = stop,
          hours = hours,
          team = team
        )
    }, 

    show_tasks = function(...) {

      data <- private$tasks |> 
        dplyr::filter(...)

      if(nrow(data) == 0) {
        cat(":)\n")
        return(invisible(NULL))
      }
      data |>
        dplyr::arrange(start) |>
        dplyr::mutate(
          start = format(start, "%A %B %d %Y"),
          stop = format(stop, "%A %B %d %Y")
        ) |>
        flextable::flextable() |> 
        flextable::autofit()
    },
    
    get_workload = function(start = lubridate::today(), 
                            stop = start + 90) {

      if(!inherits(start, "Date")) start <- parse_lazy_date(start)
      if(!inherits(stop, "Date")) stop <- parse_lazy_date(stop)
                          
      base <- tibble::tibble(date = date_vec(start, stop), hours = 0)
    
      private$tasks |>
        purrr::pmap(
          \(..., start, stop, hours) {
            tibble::tibble(date = date_vec(start, stop), hours = hours)
          }
        ) |>
        dplyr::bind_rows() |>
        dplyr::bind_rows(base) |>
        dplyr::summarise(hours = sum(hours), .by = date) |>
        dplyr::arrange(date) |>
        dplyr::mutate(
          weekday  = lubridate::wday(date, label = TRUE),
          month    = lubridate::month(date, label = TRUE),
          monthday = lubridate::mday(date),
          week     = cumsum(weekday == "Mon")
        ) |>
        dplyr::filter(!(weekday %in% c("Sat", "Sun"))) # TODO: support weekends eventually?
    },

    get_calendar = function(start = lubridate::today(), 
                            stop = start + 90) {

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
          dplyr::mutate(dayspan = span_str(monthday)) |>
          dplyr::ungroup() |>
          dplyr::select(weekday, month, hours, dayspan) |>
          tidyr::pivot_wider(names_from = weekday, values_from = hours) |> 
          dplyr::select(Month = month, Days = dayspan, Mon, Tue, Wed, Thu, Fri)
      }

      mcal <- work |> 
        dplyr::group_by(month) |> 
        dplyr::group_split() |> 
        purrr::map(work_month)
      
      mcal
    },

    show_calendar = function(start = lubridate::today(), stop = start + 90) {
      
      mcal <- self$get_calendar(start = start, stop = stop)
      mcal |> 
        dplyr::bind_rows() |>
        flextable::as_grouped_data("Month") |> 
        flextable::flextable() |> 
        flextable::autofit()
    
    }
  ),

  private = list(

    tasks = tibble::tibble(
      project = character(),
      type = character(),
      description = character(),
      start = as.Date(character(0L)),
      stop = as.Date(character(0L)),
      hours = numeric(),
      team = character()
    ),
    
    project = "project"
  )
)

#' @exportS3Method
print.calendario <- function(x, ...) {
  counts <- table(x$get_tasks()$project)
  project_name <- names(counts)
  if (length(project_name) == 0) {
    items <- "no projects or tasks"
  } else {
    project_info <- paste0("[", counts, " task", ifelse(counts == 1, "]", "s]"))
    items <- paste(project_name, project_info)
  }
  cli::cli_text("<calendario object>")
  cli::cli_ul(items)
}
