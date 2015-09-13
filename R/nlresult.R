#' Plot step measure observations
#'
#' Plot observations for each simulation step
#'
#' @param result NetLogo experiment result object
#' @param x "step_id" or measure name (as string) to choose for x axis
#' @param y measure name as string to plot on y axis
#' @param color by default it is based on "run_id" (simulation repetition).
#'   Change to \code{NA} to plot every repetition in black
#' @param x_param which parameter to use for faceting horizontally
#' @param y_param which parameter to use for faceting vertically
#' @param title plot title
#' @param data_filter optional subset expression (not quoted) using
#'   parameters, \code{run_id} and \code{step_id}
#' @param alpha lines opacity
#' @seealso To get only data and create custom plots see \code{\link{nl_get_result}}
#' @export
nl_show_step <- function(
  result, x = "step_id", y, color = "run_id",
  x_param = ".", y_param = ".", title = NULL,
  data_filter = NULL, alpha = 1) {

  if( !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  columns <- x_param
  rows <- y_param
  if( columns != "." & columns != "run_id" & !columns %in% names(result$experiment$param_sets) ) {
    stop(columns, " is not a parameter or 'run_id'")
  }
  if( rows != "." & rows != "run_id" & !rows %in% names(result$experiment$param_sets) ) {
    stop(rows, " is not a parameter or 'run_id'")
  }

  if(missing(y))
  {
    y <- names(result$experiment$measures$step)[1]
  }
  dat <- nl_get_result(result, type = "step")
  if(!missing(data_filter)) {
    e_filter <- substitute(data_filter)
    r <- eval(e_filter, dat, parent.frame())
    r <- r & !is.na(r)
    dat <- dat[r, , drop = FALSE]
  }
  dat$run_id <- factor(dat$run_id)

  if(!is.na(color)) {
    g <- ggplot2::ggplot(dat, ggplot2::aes_string(x = x, y = y, color = color))
  } else {
    g <- ggplot2::ggplot(dat, ggplot2::aes_string(x = x, y = y, z = "run_id"))
  }
  g <- g +
    ggplot2::geom_path(alpha = alpha) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none",
          panel.grid = ggplot2::element_blank(),
          panel.border = ggplot2::element_rect(color = "gray", fill = NA))
  if(rows != "." || columns != "." ) {
    g <- g + ggplot2::facet_grid(paste(rows, "~", columns), labeller = ggplot2::label_both)
  }

  if(!is.null(title)) {
    g <- g + ggplot2::labs(title = title)
  }
  g
}

#' Plot multiple patches result
#'
#' Plot patches from simualations result
#'
#' @param result NetLogo experiment result object
#' @param x_param row parameter
#' @param y_param column parameter
#' @param fill variable to control the color (default is pcolor)
#' @param type as type from nl_get_result (default is "patches_after)
#' @param sub_type as sub_type from nl_get_result (optional - if not the first patches set)
#' @export
nl_show_patches <- function(result, x_param, y_param = NULL, fill = "pcolor",
                            type = "patches_after", sub_type = NULL) {
  if( !requireNamespace("ggplot2", quietly = TRUE)) {
    stop("ggplot2 package needed for this function to work. Please install it.",
         call. = FALSE)
  }

  dat <- nl_get_result(result, type=type, sub_type = sub_type)

  if(missing(y_param)) {
    y_param <- "run_id"
    if(missing(x_param)) {
      x_param <- "run_id"
      dat[["y_param"]] <- 1L
      y_param <- "y_param"
    }
  }

  if(!x_param %in% names(dat)) {
    stop("Can't find variable ", x_param, " in data: ",
      paste(names(dat), collapse = ","))
  }
  if(!y_param %in% names(dat)) {
    stop("Can't find variable ", y_param, " in data: ",
      paste(names(dat), collapse = ","))
  }
  if(!fill %in% names(dat)) {
    stop("Can't find variable ", fill, " in data: ",
      paste(names(dat), collapse = ","))
  }

  dat[[x_param]] <- factor(dat[[x_param]])
  dat[[y_param]] <- factor(dat[[y_param]])
  dat[[fill]] <- factor(dat[[fill]])

  width <- max(dat$pxcor) - min(dat$pxcor)
  height <- max(dat$pycor) - min(dat$pycor)

  g1 <-
    ggplot2::ggplot(
      dat, ggplot2::aes_string(x= "pxcor", y = "pycor", fill = "pcolor")) +
    ggplot2::geom_raster() +
    ggplot2::coord_fixed(height/width) +
    ggplot2::facet_grid(facets = paste(y_param, "~", x_param),
                        labeller = ggplot2::label_both) +
    ggplot2::theme_minimal() +
    ggplot2::theme(axis.line = ggplot2::element_blank(),
          axis.text.x = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          axis.title.x = ggplot2::element_blank(),
          axis.title.y = ggplot2::element_blank(),
          legend.position="none")
  g1
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


  if(missing(y_param) || missing(x_param)) {
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



#' Get observations joined with parameter values
#'
#' Observations are stored in result object only with references to
#' parameter sets (param_set_id). \code{nl_get_result} joins the data
#' with actual parameters used for each observation.
#'
#' @param  result A nlexperiment result object
#' @param  add_parameters Add parameter values from parameter space to the results
#' @param  type Observation type: "run", "step", "criteria", "agents_after", "patches_after"
#'   See \code{\link{nl_run}} for simulations result structure.
#' @param  sub_type Observation sub-type (in case of individual agents measures
#'   the sub type is a name of the measure)
#' @param ... expressions to transform resulting data frame
#' @export
nl_get_result <- function(result, add_parameters = TRUE, type = "run", sub_type = NULL, ...) {

  if(!missing(sub_type) && !is.null(sub_type)) {
    res <- result[[type]][[sub_type]]
  } else {
    if(type %in% c("patches_after", "patches_before")) {
      res <- result[[type]][[1]]
    } else {
      res <- result[[type]]
    }
  }
  if(is.null(res)) {
    warning("No data in $", type, " element", call. = FALSE)
    return(NULL)
  }
  if(add_parameters > 0) {
    if(is.null(result$experiment)) {
      stop("No reference to experiment in the result")
    }
    if(is.null(result$experiment$param_sets)) {
      stop("No parameter space in referenced experiment")
    }
    param_sets <- result$experiment$param_sets
    if(nrow(param_sets) > 0) {
      param_sets$param_set_id <- seq_along(param_sets[[1]])
      if( !requireNamespace("dplyr", quietly = TRUE)) {
        res <- merge(param_sets, res, by = "param_set_id")
      } else {
        res <- dplyr::inner_join(param_sets, res, by = "param_set_id")
      }
    }
  }
  if(!missing(...)) {
    res <- transform(res, ...)
  }
  res
}

#' @rdname nl_get_result
#' @export
nl_get_run_result <- function(result, add_parameters = TRUE, ...) {

  nl_get_result(result, add_parameters, "run")
}

#' @rdname nl_get_result
#' @export
nl_get_step_result <- function(result, add_parameters = TRUE, ...) {

  nl_get_result(result, add_parameters, "step", ...)
}

#' @rdname nl_get_result
#' @export
nl_get_criteria_result <- function(result, add_parameters = TRUE, ...) {

  nl_get_result(result, add_parameters, "criteria", ...)
}

#' Calculate sensitivity according to the FAST algorithm
#'
#' Uses \code{\link[fast]{sensitivity}} from \pkg{fast} package to calculate
#' a series of model outputs according to the FAST alogrithm
#' @details
#' Only works when parameter value sets are defined with
#' \code{\link{nl_param_fast}} function.
#' Criteria must be defined in experiment (see \code{\link{nl_experiment}},
#' \code{eval_criteria} argument).
#' Sensitivity is callculated for every simulation iteration (run_id).
#' @param result A nlexperiment result object
#' @param criteria Name of evaluation criteria
#' @return A data frame with sensitivity from simulation results for every
#'   simulation repetition (run_id)
#' @export
#' @examples
#' \dontrun{
#'
#' experiment <- nl_experiment(
#'   model_file = "models/Sample Models/Biology/Flocking.nlogo",
#'   setup_commands = c("setup", "repeat 100 [go]"),
#'   iterations = 5,
#'
#'   param_values = nl_param_fast(
#'     world_size = 50,
#'     population = 80,
#'     max_align_turn = c(1, 5, 20),
#'     max_cohere_turn = c(1, 3, 20),
#'     max_separate_turn = c(1, 1.5, 20),
#'     vision = c(1, 3, 10),
#'     minimum_separation = c(1, 3, 10)
#'   ),
#'   mapping = c(
#'     max_align_turn = "max-align-turn",
#'     max_cohere_turn = "max-cohere-turn",
#'     max_separate_turn = "max-separate-turn",
#'     minimum_separation = "minimum-separation",
#'     world_size = "world-size",
#'   ),
#'   step_measures = measures(
#'     converged = "1 -
#'       (standard-deviation [dx] of turtles +
#'        standard-deviation [dy] of turtles) / 2",
#'     mean_crowding =
#'       "mean [count flockmates + 1] of turtles"
#'   ),
#'   eval_criteria = criteria(                # aggregate over iterations
#'     c_converged = mean(step$converged),
#'     c_mcrowding = mean(step$mean_crowding)
#'   ),
#'
#'   repetitions = 10,                        # repeat simulations 10 times
#'   random_seed = 1:10
#' )
#'
#' #run experiment
#' result <- nl_run(experiment, parallel = TRUE)
#'
#' #get sensitivity data
#' sensitivity_data <- nl_get_fast_sensitivity(result, "c_converged")
#' }
nl_get_fast_sensitivity <- function(result, criteria) {

  if( !requireNamespace("fast", quietly = TRUE)) {
    stop("fast package needed for nl_param_fast. Please install it",
         call. = FALSE)
  }

  dat <- nl_get_result(result, type = "criteria")
  if(is.null(dat[[criteria]]))
    stop("No criteria data in the result. Check experiment/criteria definition.")

  par_names <- names(nl_get_param_range(result$experiment)$lower)[1:6]
  sensitivity_data <- lapply(
    split(dat[[criteria]], dat$run_id ),
    fast::sensitivity, numberf = length(par_names),
    make.plot = FALSE, names = par_names)

  do.call(rbind,
          lapply( seq_along(sensitivity_data), function(i) {
            data.frame(param_sensitivity = sensitivity_data[[i]],
                       run_id = i, param = par_names)
          })
  )
}
