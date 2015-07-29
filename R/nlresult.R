
#' Show view(s) from a NetLogo result object
#'
#' @param result NetLogo result object
#' @param param_set_id Optional filter on parameter space ID
#' @param run_id Optional filter on run ID
#' @export
nl_show_view <- function(result, param_set_id = NULL, run_id = NULL) {

  row_filter <- rep(TRUE, nrow(result$export))
  if(!missing(param_set_id)) {
    row_filter <- row_filter & result$export$param_set_id %in% param_set_id
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

#' Show exported views images in a grid
#'
#' @param result Result from \code{nl_run} function
#' @param x_param Name of parameter on x axis
#' @param y_param Name of parameter on y axis
#' @param img_gap A gap between the images
#' @export
nl_show_views_grid <- function(result,
                               x_param = NULL, y_param = NULL,
                               img_gap = 0.03) {


  if( !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if( !requireNamespace("png", quietly = TRUE)) {
    stop("png package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  param_sets <- result[["experiment"]][["param_sets"]]
  if(nrow(param_sets) > 0) {
    param_sets[["param_set_id"]] <- 1:nrow(param_sets)
    dat <- merge(param_sets, result[["export"]], by = "param_set_id")
  } else {
    dat <- result[["export"]]
  }


  if(missing(y_param)) {
    y_param <- "run_id"
    if(missing(x_param)) {
      x_param <- "run_id"
      dat[["y_param"]] <- 1L
      y_param <- "y_param"
    }
  }
  x_cor <- factor(dat[[x_param]])
  y_cor <- factor(dat[[y_param]])
  x_cor_ind <- match( as.character(dat[[x_param]]), levels(x_cor))
  y_cor_ind <- match( as.character(dat[[y_param]]), levels(y_cor))

  gap <- img_gap

  g1 <-
    ggplot2::ggplot(dat, ggplot2::aes_string(
      x = sprintf("factor(%s)", x_param),
      y = sprintf("factor(%s)", y_param))) +
    ggplot2::geom_point()

  for(i in 1:nrow(dat)) {
    xmin <- x_cor_ind[i] - 0.5 + gap
    xmax <- x_cor_ind[i] + 0.5 - gap
    ymin <- y_cor_ind[i] - 0.5 + gap
    ymax <- y_cor_ind[i] + 0.5 - gap
    img <- png::readPNG(dat[i, "view"])
    g1 <- g1 +
      ggplot2::annotation_raster(img, xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  }
  g1 <- g1 +
    ggplot2::theme_minimal() +
    ggplot2::coord_fixed() +
    ggplot2::xlab(x_param) + ggplot2::ylab(y_param)

  if(length(levels(y_cor)) < 2) {
    g1 <- g1 + ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank())
  }
  if(length(levels(x_cor)) < 2) {
    g1 <- g1 + ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank())
  }
  g1
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
#' @param  add_parameters Add parameter values from parameter space to the results
#' @param  type Observation type: "run", "step", "criteria" or "export"
#' @export
nl_get_result <- function(result, add_parameters = TRUE, type = "run") {

  res <- result[[type]]
  if(is.null(res)) {
    warning("No data in $", type, " element", call. = FALSE)
    return(NULL)
  }
  if(add_parameters) {
    if(is.null(result$experiment)) {
      stop("No reference to experiment in the result")
    }
    if(is.null(result$experiment$param_sets)) {
      stop("No parameter space in referenced experiment")
    }
    param_sets <- result$experiment$param_sets
    param_sets$param_set_id <- seq_along(param_sets[[1]])
    if( !requireNamespace("dplyr", quietly = TRUE)) {
      res <- merge(param_sets, res, by = "param_set_id")
    } else {
      res <- dplyr::inner_join(param_sets, res, by = "param_set_id")
    }
  }

  res
}
