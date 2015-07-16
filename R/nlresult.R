
#' Show view(s) from a NetLogo result object
#'
#' @param result NetLogo result object
#' @param param_space_id Optional filter on parameter space ID
#' @param run_id Optional filter on run ID
#' @export
nl_show_view <- function(result, param_space_id = NULL, run_id = NULL) {

  row_filter <- rep(TRUE, nrow(result$export))
  if(!missing(param_space_id)) {
    row_filter <- row_filter & result$export$param_space_id == param_space_id
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
#' @param  param_space_id Optional filter on parameter space ID
#' @param  run_id Optional filter on run ID
#' @export
nl_get_run_result <- function(result, param_space_id = NULL, run_id = NULL) {

  if( !requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package needed for this function to work. Please install it.",
         call. = FALSE)
  }

  param_space <- dplyr::mutate(
    result$experiment$param_space, param_space_id = dplyr::row_number())

  res <- dplyr::inner_join(param_space, result$run, by = "param_space_id")

  if(!missing(param_space_id)) res <- dplyr::filter(res, param_space_id = param_space_id)
  if(!missing(run_id)) res <- dplyr::filter(res, param_space_id = run_id)
  res
}

