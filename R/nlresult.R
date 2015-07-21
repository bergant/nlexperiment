
#' Show view(s) from a NetLogo result object
#'
#' @param result NetLogo result object
#' @param param_space_id Optional filter on parameter space ID
#' @param run_id Optional filter on run ID
#' @export
nl_show_view <- function(result, param_space_id = NULL, run_id = NULL) {

  row_filter <- rep(TRUE, nrow(result$export))
  if(!missing(param_space_id)) {
    row_filter <- row_filter & result$export$param_space_id %in% param_space_id
  }
  if(!missing(run_id)) {
    row_filter <- row_filter & result$export$run_id == run_id
  }

  img_files <- result$export[row_filter, "view"]

  if( !requireNamespace("png", quietly = TRUE)) {
    stop("png package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  for(img_file in img_files) {
    img1 <- png::readPNG(img_file)
    grid::grid.newpage()
    grid::grid.raster(img1)
  }
  invisible(img_files)
}

#' Get run observations joined with parameter space values
#'
#' @param  result NetLogo result object
#' @param  add_parameters If parameter values should be appended to the results
#' @export
nl_get_run_result <- function(result, add_parameters = TRUE) {

  nl_get_result(result, add_parameters, "run")
}

#' Get step observations joined with parameter space values
#'
#' @param  result NetLogo result object
#' @param  add_parameters If parameter values should be appended to the results
#' @export
nl_get_step_result <- function(result, add_parameters = TRUE) {

  nl_get_result(result, add_parameters, "step")
}

#' Get step observations joined with parameter space values
#'
#' @param  result NetLogo result object
#' @param  add_parameters If parameter values should be appended to the results
#' @param  type Observation type: "run" or "step"
#' @export
#' @keywords internal
nl_get_result <- function(result, add_parameters = TRUE, type = "run") {

  res <- if(type == "run") res <- result$run else res <- result$step

  if(add_parameters) {
    param_space <- result$experiment$param_space
    param_space$param_space_id <- seq_along(param_space[[1]])
    if( !requireNamespace("dplyr", quietly = TRUE)) {
      res <- merge(param_space, res, by = "param_space_id")
    } else {
      res <- dplyr::inner_join(param_space, res, by = "param_space_id")
    }
  }

  res
}
