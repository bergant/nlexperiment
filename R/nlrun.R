#' Run NetLogo experiment
#'
#' @param experiment NetLogo experiment object
#' @param gui Start NetLogo with GUI (by default NetLogo is run in headless mode)
#' @param print_progress Set to TRUE if you want to follow the progress in the console
#' @param parallel Runs experiment in parallel worker processes
#'   (requires \link[parallel]{parallel} package)
#' @return Returns a list of values which depend on measures defined for each step
#'   and/or for each run, parameter space, repetitions and export options.
#'   \itemize{
#'   \item Element \code{step} is a result of step measures.
#'   \item Element \code{run} is a result of run measures.
#'   \item Element \code{export} contains file names of exported views
#'   and worlds.
#'   \item Element \code{duration} is a time spent to complete the experiment.
#'   \item Element \code{experiment} is the NetLogo experiment object used for simulation.
#'   }
#'
#' @details Model is run for each parameter combination
#'   defined in parameter space. If \code{repetition} (defined in experiment)
#'   is greater than \code{1} then
#'   each parameter combination is repeated accordingly.
#'
#'   Use parallel option if there are more than a few runs per processor core.
#' @seealso See \code{\link{nl_experiment}} for creating NetLogo experiment object.
#' @export
nl_run <- function(experiment, print_progress = FALSE, gui = FALSE, parallel = FALSE) {
  if(parallel) {
    return(nl_run_parallel(experiment, print_progress, gui))
  }
  if(is.null(nl_path <- nl_netlogo_path())) {
    stop("NetLogo path is empty. Use nl_netlogo_path.")
  }
  on.exit(nl_run_end())
  nl_run_init(gui = gui, nl_path = nl_path, model_path = experiment$model_file)

  start_time <- Sys.time()

  run_schedule <- nl_run_schedule(experiment)

  ret <- lapply(seq_len(nrow(run_schedule)),
                function(i) {
                  nl_single_run(
                    experiment,
                    parameter_space_id = run_schedule[i, "param_space_id"],
                    run_id = run_schedule[i, "run_id"],
                    print_progress
                  )
                }
  )

  ret <- nl_run_wrap_results(experiment, ret, start_time)
}


nl_run_parallel <- function(experiment, print_progress = FALSE, gui = FALSE) {

  if( !requireNamespace("parallel", quietly = TRUE)) {
    stop("parallel package needed for this function to work. Please install it.",
         call. = FALSE)
  }
  if(is.null(nl_path <- nl_netlogo_path())) {
    stop("NetLogo path is empty. Use nl_netlogo_path.")
  }

  processors <- parallel::detectCores()
  cl <- parallel::makeCluster(processors)

  on.exit({
    invisible(
      parallel::parLapply(cl, 1:processors, nl_run_end)
    )
    parallel::stopCluster(cl)
  })

  invisible(
    parallel::parLapply(
      cl, 1:processors, nl_run_init,
      gui=gui, nl_path = nl_path, model_path = experiment$model_file)
  )


  start_time <- Sys.time()

  run_schedule <- nl_run_schedule(experiment)

  ret <- parallel::parLapply(cl, seq_len(nrow(run_schedule)),
                             function(i) {
                               nl_single_run(
                                 experiment,
                                 parameter_space_id = run_schedule[i, "param_space_id"],
                                 run_id = run_schedule[i, "run_id"],
                                 print_progress
                               )
                             }
  )

  ret <- nl_run_wrap_results(experiment, ret, start_time)
}

nl_run_schedule <- function(experiment) {
  # get run schedule based on parameter space and number of iterations
  if(is.null(experiment$param_space) || nrow(experiment$param_space) == 0) {
    warning("Parameter space not defined. Using default parameters", call. = FALSE)
    param_space_rows <- NA
  } else {
    param_space_rows <- seq_len(nrow(experiment$param_space))
  }

  expand.grid(
    param_space_id = param_space_rows,
    run_id = seq_len(experiment$run_options$repetitions))
}

nl_run_wrap_results <- function(experiment, ret, start_time) {
  # concatenate results to dataframes
  step_df <- do.call(rbind, lapply(ret, function(x) x$step))
  run_df <- do.call(rbind, lapply(ret, function(x) x$run))
  export_df <- do.call(rbind, lapply(ret, function(x) x$export))
  ret <- list(step = step_df, run = run_df, export = export_df)

  # report duration
  ret$duration <- Sys.time() - start_time
  ret$experiment <- experiment
  class(ret) <- c(nl_result_class, class(ret))
  ret
}

nl_run_init <- function(dummy = NULL, gui, nl_path, model_path) {
  #Start NetLogo and load model
  nl_options$set("wd", getwd())
  RNetLogo::NLStart(nl_path, gui=gui)
  RNetLogo::NLLoadModel(model_path)
}

nl_run_end <- function(x) {
  #Close NetLogo
  RNetLogo::NLQuit()
}

nl_single_run <- function(experiment, parameter_space_id = NULL, run_id = NULL,
                          print_progress = FALSE) {
  if(print_progress) {
    cat("Params: ", parameter_space_id, ", Run: ", run_id, "\n", sep = "")
  }
  start_time <- Sys.time()

  # set random seed
  if(!is.null(experiment$run_options$random_seed)) {
    RNetLogo::NLCommand(sprintf("random-seed %d", experiment$run_options$random_seed))
  }
  # set parameters from parameter space
  if(!missing(parameter_space_id) && !is.null(experiment$param_space)) {
    # set world size if specified
    if(!is.null(experiment$param_space[["world_size"]])) {
      world_size <- experiment$param_space[parameter_space_id, "world_size"]
      half_size <-  world_size %/% 2
      RNetLogo::NLCommand(sprintf("resize-world %d %d %d %d",
                                  -half_size, half_size, -half_size, half_size))
    }
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
