
# package options (used for NetLogo path and other session settings)
nl_options_class <- function() {
  options <- list()
  set <- function(key, value) {
    options[[key]] <<- value
  }
  get <- function(key) {
    options[[key]]
  }
  return(list(set = set, get = get))
}
nl_options <- nl_options_class()

# package global variables
nl_experiment_class <- "nl_experiment"
nl_result_class <- "nl_result"
nl_special_params <- c("world_size")


#' Get and set netlogo path
#'
#' @param nl_path An absolute path to your NetLogo installation
#'   (the folder where the NetLogo.jar is) starting from the root.
#'   On Windows, for example, something like "C:/Program Files/NetLogo 5.1.0".
#' @export
nl_netlogo_path <- function(nl_path = NULL) {
  if(missing(nl_path)) {
    nl_options$get("nl_path")
  } else {
    nl_options$set("nl_path", nl_path)
  }
}

#' Get and set export path
#'
#' @param export_path target folder to export files
#' @export
nl_export_path <- function(export_path = NULL) {
  if(missing(export_path)) {
    nl_options$get("export_path")
  } else {
    nl_options$set("export_path", export_path)
  }
}

#' Create NetLogo experiment object
#'
#' @param model_file An absolute path to your NetLogo model file (.nlogo)
#' @param max_ticks Number of iterations to run.
#'    This parameter is used when while_condition is not defined.
#' @param while_condition A string with a NetLogo conditional reporter.
#'     (for example: "ticks < 100")
#' @param repetitions How many times to run the model with the same parameters.
#'   It is set to 1 by default. Result data sets will include run_id as
#'   additional variable to identify the specific runs.
#' @param step_measures Measures per each simulation step in a named character
#'   vector. Use measures() function to construct measures in right format.
#' @param run_measures Measures per each simulation run in a named character
#'   vector. Use measures() function to construct measures in right format.
#' @param param_values A data.frame with parameter values or
#'   a list of values to be expanded to all combinations of values
#' @param mapping Mapping between R and NetLogo parameters
#'   in named character vector.
#'   For example: c(diffusion_rate = "diffusion-rate", population = "population")
#' @param export_view If set to TRUE, the views will be exported to
#'   a png image files for each run (when running the experiment)
#' @param export_world If set to TRUE, the world will be exported to
#'   a csv file for each run
#' @return NetLogo experiment object
#' @export
nl_experiment <- function(model_file,
                          max_ticks = NULL,
                          while_condition = NULL,
                          repetitions = 1,
                          step_measures = NULL,
                          run_measures = NULL,
                          mapping = NULL,
                          param_values = NULL,
                          export_view = FALSE,
                          export_world = FALSE
                          ) {
  if(missing(while_condition) && !missing(max_ticks)) {
    while_condition <- sprintf("ticks < %s", max_ticks)
  }
  experiment <- list(
    model_file = model_file,
    while_condition = while_condition,
    mapping = mapping,
    export_view = export_view,
    export_world = export_world
  )
  class(experiment) <- c(nl_experiment_class, class(experiment))


  # set default run options (1000 iterations)
  experiment <- nl_set_run_options(experiment, repetitions = repetitions)
  # set default measures (no measures)
  experiment <- nl_set_measures(experiment,
                                step = step_measures,
                                run = run_measures)
  # set default param space (empty data.frame)
  experiment <- nl_set_param_space(experiment, param_values = param_values)

  return(experiment)
}

#' Convert measure list to named vector
#'
#' @param ... Named character values
#' @export
#' @keywords internal
measures <- function(...) {
  unlist(list(...))
}

#' Set run options to a NetLogo experiment
#'
#' @param experiment NetLogo experiment object from nl_experiment() function
#' @param random_seed Random seed
#' @param repetitions Number of repetitions (when random seed is not defined)
#' @param max_minutes If max.minutes > 0 the execution stops after the
#'     defined number of minutes (with an error and no return value)
#' @param setup_commands NetLogo command strings to execute to setup the model
#' @param go_command NetLogo command string to execute the step in the model
#' @return NetLogo experiment object
#' @export
nl_set_run_options <- function(
  experiment,
  random_seed = NULL,
  repetitions = 1,
  max_minutes = 10,
  setup_commands = "setup",
  go_command = "go"
) {

  if(!inherits(experiment, nl_experiment_class))
    stop("Not a NetLogo experiment object")


  experiment$run_options <- list(
    random_seed = random_seed,
    repetitions = repetitions,
    max_minutes = max_minutes,
    setup_commands = setup_commands,
    go_command = go_command
  )
  experiment
}


#' Add measures for NetLogo experiment
#'
#' @param experiment NetLogo model object from nl_experiment() function
#' @param step NetLogo reporters for each step (reported at every tick)
#' @param run NetLogo reporters for each run (reported at end of run)
#' @param as.data.frame Reporting in data frame format
#' @param step_transform A function to transform data frame result from
#'   step reporters.
#' @return NetLogo experiment object
#' @export
nl_set_measures <- function(experiment,
                        step = NULL,
                        run = NULL,
                        as.data.frame = TRUE,
                        step_transform = NULL) {
  if(!inherits(experiment, nl_experiment_class))
    stop("Not a NetLogo experiment object")

  experiment$measures <-
    list(step = step,
         run = run,
         as.data.frame = as.data.frame,
         step_transform = step_transform)

  experiment
}

#' Define parameter space for NetLogo experiment
#'
#' @param experiment NetLogo experiment object from nl_experiment() function
#' @param param_values A data.frame with parameter values or
#'   a list of values to be expanded to all combinations of values
#' @return NetLogo experiment object
#' @export
nl_set_param_space <- function(experiment, param_values = NULL ) {
  if(!inherits(experiment, nl_experiment_class))
    stop("Not a NetLogo experiment object")

  if(missing(param_values) || is.null(param_values)) {
    param_space <- data.frame()
  } else if(inherits(param_values, "data.frame")) {
    param_space <- param_values
  } else if(inherits(param_values, "list")) {
    param_space <- expand.grid(param_values)
  } else {
    stop("Attribute param_values should be a data frame or a list")
  }
  experiment$param_space <- param_space
  experiment
}



#' Run NetLogo experiment
#'
#' @param experiment NetLogo experiment object
#' @param gui Start NetLogo with GUI (by default NetLogo is run in headless mode)
#' @return A list of data frames with observations (per run and per simulation step)
#' @details NetLogo is started and the model is run for each tuple of parameters
#'   defined in parameter space. When repetition (in run options) is > 1 than
#'   each tuple run is repeated accordingly.
#'   The result depends on measures defined for each step and for each run.
#' @export
nl_run <- function(experiment, gui = FALSE) {

  nl_options$set("wd", getwd())

  # start NetLogo
  if(is.null(nl_path <- nl_netlogo_path())) {
    stop("NetLogo path is empty. Use nl_netlogo_path.")
  }
  nl_instance <- RNetLogo::NLStart(nl_path, gui = gui)
  on.exit(RNetLogo::NLQuit())
  RNetLogo::NLLoadModel( experiment$model_file )

  # run for every parameter set in parameter space
  if(is.null(experiment$param_space) || nrow(experiment$param_space) == 0) {
    warning("No parameter space defined. Using default parameters",
            call. = FALSE)
    param_space_rows <- NA
  } else {
    param_space_rows <- seq_len(nrow(experiment$param_space))
  }
  start_time <- Sys.time()
  ret <- lapply(param_space_rows,
                function(param_space_id)
                  nl_run_param(experiment, param_space_id)
  )

  # concatenate results to dataframes
  step_df <- do.call(rbind, lapply(ret, function(x) x$step))
  run_df <- do.call(rbind, lapply(ret, function(x) x$run))
  export_df <- do.call(rbind, lapply(ret, function(x) x$export))
  ret <- list(step = step_df, run = run_df, export = export_df)

  # report duration
  ret$duration <- Sys.time() - start_time
  ret$experiment <- experiment
  class(ret) <- c(nl_result_class, class(ret))
  return(ret)
}

nl_run_param <- function(experiment, param_space_id) {
  # run repetitions for same parameters
  rret <- lapply(seq_len(experiment$run_options$repetitions),
                 function(run_id) {
                   nl_single_run(experiment, param_space_id, run_id)
                 })
  #rbind result dataframes
  step_df <- do.call(rbind, lapply(rret, function(x) x$step))
  run_df <- do.call(rbind, lapply(rret, function(x) x$run))
  export_df <- do.call(rbind, lapply(rret, function(x) x$export))

  return(list(step = step_df, run = run_df, export = export_df))
}

nl_single_run <- function(experiment, parameter_space_id = NULL, run_id = NULL) {
  start_time <- Sys.time()
  # set parameters from parameter space
  if(!missing(parameter_space_id) && !is.null(experiment$param_space)) {
    param_names <- setdiff(names(experiment$param_space), nl_special_params)
    for(parameter in param_names) {
      nl_param <- experiment$mapping[parameter]
      if(is.null(nl_param) || is.na(nl_param)) {
        nl_param <- parameter
      }
      param_value <- experiment$param_space[parameter_space_id, parameter]
      RNetLogo::NLCommand(sprintf("set %s %s", nl_param, param_value))
    }
  }

  # execute setup command(s)
  for(command in experiment$run_options$setup_commands) {
    RNetLogo::NLCommand(command)
  }

  # run
  if(length(experiment$measures$step) > 0 ) {
    # if any step measures defined - use RNetLogo::NLDoReportWhile
    report_step <- RNetLogo::NLDoReportWhile(
      condition = experiment$while_condition,
      command = experiment$run_options$go_command,
      reporter = experiment$measures$step,
      as.data.frame = experiment$measures$as.data.frame,
      df.col.names = names(experiment$measures$step),
      max.minutes = experiment$run_options$max_minutes
    )
    report_step$tick <- as.numeric(row.names(report_step))
    report_step$run_id <- run_id
    report_step$param_space_id <- parameter_space_id
    if(!is.null(experiment$measures$step_transform)) {
      report_step <- experiment$measures$step_transform(report_step)
    }
  } else {
    # no step measures - just run the model
    RNetLogo::NLDoCommandWhile(
      condition = experiment$while_condition,
      experiment$run_options$go_command,
      max.minutes = experiment$run_options$max_minutes
    )
    report_step <- NULL
  }
  # compute measures defined per run
  if(length(experiment$measures$run) > 0) {
    report_run <- RNetLogo::NLReport(experiment$measures$run)
    if(experiment$measures$as.data.frame) {
      report_run <- data.frame(report_run)
      names(report_run) <- names(experiment$measures$run)
      report_run$param_space_id <- parameter_space_id
      report_run$run_id <- run_id
      report_run$run_duration <- Sys.time() - start_time
    }
  } else {
    report_run <- NULL
  }
  # exports
  if(any(experiment$export_view, experiment$export_world)){
    export <- nl_single_export(experiment, parameter_space_id, run_id)
  } else {
    export <- NULL
  }
  return(list(step = report_step, run = report_run, export = export))
}

#' Export view and/or world for individual run
#'
#' @param experiment experiment object
#' @param experiment experiment object
#' @keywords internal
nl_single_export <- function(experiment, param_space_id = NA, run_id = 1) {


  file_path <- nl_export_path()
  if(is.null(file_path)) {
    file_path <- nl_options$get("wd")
    file_path <- file.path(file_path, "export")
    if(!dir.exists(file_path)) dir.create(file_path)
  }
  if(is.null(file_path)) file_path <- ""
  ret <- data.frame(param_space_id = param_space_id,
                    run_id = run_id,
                    view = NA,
                    world = NA)
  if(experiment$export_view) {
    filename <- sprintf("view_%d_%d.png",
                        ifelse(is.null(param_space_id), 1, param_space_id),
                        ifelse(is.null(run_id), 1, run_id))
    view_filename <- file.path(file_path, filename)
    RNetLogo::NLCommand(sprintf('export-view "%s"', view_filename))
    ret$view <- view_filename
  }

  if(experiment$export_world) {
    filename <- sprintf("world_%d_%d.csv",
                        ifelse(is.null(param_space_id), 1, param_space_id),
                        ifelse(is.null(run_id), 1, run_id))
    world_filename <- file.path(file_path, filename)
    RNetLogo::NLCommand(sprintf('export-world "%s"', world_filename))
    ret$world <- world_filename
  }
  ret
}

#' Print a NetLogo experiment object
#'
#' @param x NetLogo experiment object
#' @param ... further arguments passed to or from other methods.
#' @export
print.nl_experiment <- function(x, ...) {
  if(!inherits(x, nl_experiment_class))
    stop("Not a NetLogo experiment object")

  with(x, {
    cat("NetLogo experiment object - ", basename(model_file), "\n")
    cat("Model: ", model_file, "\n")
    cat("Run condition: ", while_condition, "\n")
    cat("Run options:\n")
    if(!is.null(run_options$random_seed)) {
      cat("  Random seed", run_options$random_seed, "\n")
    }
    if(run_options$repetitions > 1) {
      cat("  Repetitions", run_options$repetitions, "\n")
    }
    cat("  Setup procedures: \n    ")
    cat(run_options$setup_commands, sep = "\n    ")
    cat("  Go command: ", run_options$go_command, "\n")
    cat("Step measures: ", names(measures$step), "\n")
    cat("Run measures: ", names(measures$run), "\n")
    cat("Parameter space:\n")
    cat("  Size: ", nrow(param_space), "\n")
    cat("  Parameters: ", names(param_space))
  })
}

