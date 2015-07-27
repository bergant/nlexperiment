#' Run steps (internal doc)
#'
#' \itemize{
#' \item nl_run | nl_run_parallel
#'   \itemize{
#'     \item nl_run_init
#'     \item nl_single_run
#'     \itemize{
#'       \item nl_single_run_setup setting parameters, setup commands
#'       \item execute NLDoReportWhile | NLDoCommandWhile
#'       \item get run measures
#'       \item nl_single_agent_report
#'       \item nl_single_export
#'       \item calling external data handler
#'     }
#'     \item nl_run_wrap_results
#'     \item nl_run_end
#'   }
#' }
#'
#' @name run.steps
#' @keywords internal
NULL

#' Run NetLogo experiment
#'
#' @param experiment NetLogo experiment object
#' @param gui Start NetLogo with GUI (by default NetLogo is run in headless mode)
#' @param print_progress Set to TRUE if you want to follow the progress in the console
#' @param parallel Runs experiment in parallel worker processes
#'   (requires \link[parallel]{parallel} package)
#' @param max_cores (optional) only relevant if parallel = TRUE.
#'   If not defined all available processors will be used
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
nl_run <- function(experiment, print_progress = FALSE, gui = FALSE,
                   parallel = FALSE, max_cores = NULL) {
  if(is.null(nl_path <- nl_netlogo_path())) {
    stop("NetLogo path is empty. Use nl_netlogo_path.")
  }
  if(!file.exists(experiment$model_file)) {
    stop("File does not exist ", experiment$model_file)
  }
  if(parallel) {
    return(nl_run_parallel(experiment, nl_path, print_progress, gui, max_cores))
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



nl_run_parallel <- function(experiment, nl_path, print_progress = FALSE,
                            gui = FALSE,
                            max_cores = NULL) {

  if( !requireNamespace("parallel", quietly = TRUE)) {
    stop("parallel package needed for this function to work. Please install it.",
         call. = FALSE)
  }

  processors <- parallel::detectCores()
  if(!missing(max_cores)) {
    processors <- min(processors, max_cores)
  }
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
  if(is.null(experiment$run_options$data_handler)) {
    step_df <- do.call(rbind, lapply(ret, function(x) x$step))
    run_df <- do.call(rbind, lapply(ret, function(x) x$run))
    export_df <- do.call(rbind, lapply(ret, function(x) x$export))
    ret1 <- list(step = step_df, run = run_df, export = export_df)

    if(!is.null(ret[[1]]$agents_before)) {
      ret1$agents_before <- nl_run_wrap_result_agents(ret, "agents_before")
    }
    if(!is.null(ret[[1]]$agents_after)) {
      ret1$agents_after <- nl_run_wrap_result_agents(ret, "agents_after")
    }
    if(!is.null(ret[[1]]$patches_after)) {
      ret1$patches_after <- nl_run_wrap_result_agents(ret, "patches_after")
    }
    if(!is.null(ret[[1]]$patches_before)) {
      ret1$patches_before <- nl_run_wrap_result_agents(ret, "patches_before")
    }
  }
  else
  {
    ret1 <- list()
  }

  # report duration
  ret1$duration <- Sys.time() - start_time
  ret1$experiment <- experiment
  class(ret1) <- c(nl_result_class, class(ret1))
  ret1
}

nl_run_wrap_result_agents <- function(ret, type) {
    setNames(
      lapply(seq_along(ret[[1]][[type]]), function(i) {
        do.call(rbind, lapply(ret, function(x) {x[[type]][[i]]}))
      }),
      names(ret[[1]][[type]])
    )
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

  # set up parameters and setup NetLogo commands
  nl_single_run_setup(experiment, parameter_space_id, run_id, print_progress)

  #agents before
  agents_before <- NULL
  if(!is.null(experiment$agents_before)) {
    agents_before <-
      nl_single_agent_report(experiment$agents_before,
                             parameter_space_id,
                             run_id)
  }
  #patches before
  patches_before <- NULL
  if(!is.null(experiment$patches_before)) {
    patches_before <-
      nl_single_patch_report(experiment$patches_before,
                             parameter_space_id,
                             run_id)
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
  ret <- list(step = report_step, run = report_run)

  #agents

  if(!is.null(experiment$agents_after)) {
    ret$agents_after <-
      nl_single_agent_report(experiment$agents_after,
                             parameter_space_id,
                             run_id)
  }
  if(!is.null(agents_before)) {
    ret$agents_before <- agents_before
  }

  # patches
  if(!is.null(experiment$patches_after)) {
    ret$patches_after <-
      nl_single_patch_report(experiment$patches_after,
                             parameter_space_id,
                             run_id)
  }
  if(!is.null(patches_before)) {
    ret$patches_before <- patches_before
  }

  # exports
  if(any(experiment$export_view, experiment$export_world)){
    ret$export <- nl_single_export(experiment, parameter_space_id, run_id)
  }

  # if external data handler is defined
  if(!is.null(experiment$run_options$data_handler)) {
    experiment$run_options$data_handler(ret)
    ret <- NULL
  }

  return(ret)
}

#' Set up parameters and setup commands before run
#'
#' Internal function - called from nl_single_run
#'
#' @param experiment see nl_single_run
#' @param parameter_space_id see nl_single_run
#' @param run_id see nl_single_run
#' @keywords internal
nl_single_run_setup <- function(experiment, parameter_space_id = NULL, run_id = NULL,
                                print_progress = FALSE) {

  # set random seed
  if(!is.null(experiment$run_options$random_seed)) {
    rseed <- experiment$run_options$random_seed
    if(length(experiment$run_options$random_seed) > 0) {
      rseed <- rseed[min(run_id,length(rseed))]
    }
    RNetLogo::NLCommand(sprintf("random-seed %d", rseed))
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
    # set other parameters
    param_names <- setdiff(names(experiment$param_space), nl_special_params)
    for(parameter in param_names) {
      nl_param <- experiment$mapping[parameter]
      if(is.null(nl_param) || is.na(nl_param)) {
        nl_param <- parameter
      }
      if(nl_param != "") {
        param_value <- experiment$param_space[parameter_space_id, parameter]
        RNetLogo::NLCommand(sprintf("set %s %s", nl_param, param_value))
      }
    }
  }

  # execute setup command(s)
  for(command in experiment$run_options$setup_commands) {
    RNetLogo::NLCommand(command)
  }
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
    fhash <- digest::digest(experiment, algo = "murmur32")

    filename <- sprintf("view_%s_%d_%d.png",
                        fhash,
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

nl_single_agent_report <- function(agent_report,
                                   parameter_space_id = NA,
                                   run_id = 1) {

  lapply(agent_report, function(x) {
    df1 <- RNetLogo::NLGetAgentSet(x$vars, x$agents)
    if(!is.null(names(x$vars))) names(df1) <- names(x$vars)
    df1$run_id <- run_id
    df1$param_space_id <- parameter_space_id
    df1
  })
}

nl_single_patch_report <- function(patch_report,
                                   parameter_space_id = NA,
                                   run_id = 1) {

  lapply(patch_report, function(x) {
    df1 <- RNetLogo::NLGetPatches(x$vars, x$patches)
    if(!is.null(names(x$vars))) names(df1) <- names(x$vars)
    df1$run_id <- run_id
    df1$param_space_id <- parameter_space_id
    df1
  })
}
