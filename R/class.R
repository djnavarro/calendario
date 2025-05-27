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

      base <- tibble::tibble(date = as.Date(start:stop), hours = 0)
    
      private$tasks |>
        purrr::pmap(\(..., start, stop, hours) tibble::tibble(date = as.Date(start:stop), hours = hours)) |>
        dplyr::bind_rows() |>
        dplyr::bind_rows(base) |>
        dplyr::summarise(hours = sum(hours), .by = date) |>
        dplyr::arrange(date) |>
        dplyr::mutate(
          wday = lubridate::wday(date, label = TRUE),
          mday = lubridate::mday(date),
          wnum = cumsum(wday == "Mon"),
          month = lubridate::month(date, label = TRUE)
        ) |>
        dplyr::filter(!(wday %in% c("Sat", "Sun")))
    },

    get_calendar = function(start = lubridate::today(), 
                            stop = start + 90,
                            split = TRUE) {

      work <- self$get_workload(start = start, stop = stop)

      cal <- work |>
        dplyr::group_by(wnum) |>
        dplyr::mutate(wstr = paste(range(mday), collapse="-")) |>
        dplyr::ungroup() |>
        dplyr::select(wday, month, hours, wstr) |>
        tidyr::pivot_wider(names_from = wday, values_from = hours) |> 
        tidyr::unite(month, wstr, col = "Week", sep = " ", remove = FALSE) |> 
        dplyr::select(Week, Mon, Tue, Wed, Thu, Fri, Month = month)
      
      if(split) {
        cal <- cal |> 
          dplyr::group_by(Month) |> 
          dplyr::group_split() |> 
          purrr::map(\(x) x |> dplyr::select(-Month))
      }

      cal
    },

    show_calendar = function(start = lubridate::today(), stop = start + 90) {
      
      cal <- self$get_calendar(start = start, stop = stop, split = FALSE)
      cal |> 
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
  project_info <- paste0("[", counts, " task", ifelse(counts == 1, "]", "s]"))
  items <- paste(project_name, project_info)
  cli::cli_text("<calendario object>")
  cli::cli_ul(items)
}

#' @export
project <- function(x, project) {
  x$set_project(project)
  x
}

#' @export
task <- function(x, ...) {
  x$add_task(...)
  x
}