
# package options
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


#' Run NetLogo experiment
#'
#' Runs NetLogo model for defined every parameter and repetitions. Returns
#'   a list of data frames for each measure defined in experiment.
#'
#' @param experiment NetLogo experiment object
#' @param gui Start NetLogo with GUI (by default NetLogo is run in headless mode)
#' @param print_progress Set to TRUE if you want to follow the progress in the console
#' @param parallel Runs experiment in parallel worker processes
#'   (requires \link[parallel]{parallel} package)
#' @param max_cores (optional) only relevant if parallel = TRUE.
#'   If not defined all available processors will be used
#' @return Returns an object of class \code{nl_result}.
#'   It is a list containing at most the following components:
#'   \item{ step }{a data frame with observations based on temporal (step) measures.
#'     It includes at least
#'     param_set_id (id of parameter set),
#'     run_id (ID of simulation repetition ),
#'     step_id (ID of simulation step ),
#'     and columns named after the temporal measures}
#'   \item{ run }{a data frame with observations based on final run measures.
#'     It includes at least
#'     param_set_id (id of parameter set),
#'     run_id (ID of simulation repetition ),
#'     and columns named after the temporal measures}
#'   \item{ agents_after }{a data frame with observations based on agents
#'     after each simulation run }
#'   \item{ agents_before }{a data frame with observations based on agents
#'     before each simulation run}
#'   \item{ patches_after }{a data frame with observations based on patches
#'     after each simulation run }
#'   \item{ patches_before }{a data frame with observations based on patches
#'     before each simulation run}
#'   \item{ criteria }{a data frame with values provided by
#'   criteria expressions (\code{eval_criteria} in experiment definition
#'   possibly aggregated by \code{eval_aggregate_fun})  and additional
#'   criteria defined by \code{eval_mutate} expressions
#'   }
#'   \item{ export }{a filename list with reference to
#'     parameter sets and simulation repetitions}
#'   \item{ duration }{time spent to complete the experiment (in \code{\link{difftime}})}
#'   \item{ experiment }{original NetLogo experiment object used}
#'
#' @details Model is run for each parameter combination
#'   defined in parameter sets If \code{repetition} (defined in experiment)
#'   is greater than \code{1} then
#'   each run for a parameter set is repeated accordingly.
#'   Before each run the parameters are set and setup procedure(s) are called.
#'   After each run criteria function(s) are calculated (if defined)
#'
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

  # get schedule (simulation plan)
  run_schedule <- nl_get_schedule(experiment)
  # run schedule
  ret <- nl_run_schedule(experiment, run_schedule, print_progress)

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
    parallel::clusterCall(cl, nl_run_end)
    parallel::stopCluster(cl)
  })

  parallel::clusterCall(cl, nl_run_init, gui=gui, nl_path = nl_path,
                        model_path = experiment$model_file)

  run_schedule <- nl_get_schedule(experiment)

  nl_run_schedule(experiment, run_schedule, parallel = TRUE, cluster = cl)
}


nl_run_schedule <- function(experiment, run_schedule,
                            print_progress = FALSE,
                            parallel = FALSE, cluster = NULL) {
  # run simulation for every row in a schedule
  do_one <- function(i) {
    nl_single_run(
      experiment = experiment,
      parameter_set_id = run_schedule[i, "param_set_id"],
      run_id = run_schedule[i, "run_id"],
      param_set = run_schedule[
        i, !names(run_schedule) %in% c("param_set_id","run_id"), drop = FALSE],
      print_progress = print_progress
    )
  }

  start_time <- Sys.time()

  if(!parallel) {
    ret <- lapply(seq_len(nrow(run_schedule)), do_one)
  } else {
    ret <- parallel::parLapply(cluster, seq_len(nrow(run_schedule)), do_one)
  }

  ret <- nl_run_wrap_results(experiment, ret, start_time)
}

nl_get_schedule <- function(experiment, param_sets = NULL) {
  # get run schedule based on parameter sets and number of iterations
  if(missing(param_sets)) {
    param_sets <- experiment$param_sets
  }
  if(is.null(param_sets) || nrow(param_sets) == 0) {
    warning("Parameter sets not defined. Using default parameters", call. = FALSE)
    param_sets_rows <- NA
  } else {
    param_sets_rows <- seq_len(nrow(param_sets))
  }
  if(nrow(param_sets)>0) {
    param_sets$param_set_id <- param_sets_rows
    sch <- expand.grid(
      param_set_id = param_sets_rows,
      run_id = seq_len(experiment$run_options$repetitions))
    ret <- merge(sch, param_sets, by = "param_set_id")
  } else
  {
    ret <- data.frame(param_set_id = 1, run_id = seq_len(experiment$run_options$repetitions))
  }
  ret
}

nl_run_wrap_results <- function(experiment, ret, start_time) {
  # concatenate results to dataframes
  if(is.null(experiment$run_options$data_handler)) {
    step_df <- do.call(rbind, lapply(ret, function(x) x$step))
    run_df <- do.call(rbind, lapply(ret, function(x) x$run))
    export_df <- do.call(rbind, lapply(ret, function(x) x$export))
    criteria_df <- do.call(rbind, lapply(ret, function(x) x$criteria))

    # aggregate criteria if there is aggregation function (eval_aggregate_fun)
    if(!is.null(experiment$measures$eval_aggregate_fun)) {
      value_names <- !names(criteria_df) %in% c("param_set_id", "run_id")
      criteria_df <-
        aggregate(criteria_df[,value_names, drop = FALSE],
                  by = list(param_set_id = criteria_df$param_set_id) ,
                  FUN = experiment$measures$eval_aggregate_fun)
    }
    # mutate evaluation criteria (eval_mutate)
    if(!is.null(experiment$measures$eval_mutate)) {
      c_names <- names(experiment$measures$eval_mutate[-1])
      c_values <- lapply(experiment$measures$eval_mutate[-1],
                   function(x) with(criteria_df, eval(x)))
      c_values <- data.frame(c_values)
      criteria_df <- cbind(criteria_df, c_values)
    }
    ret1 <- list(step = step_df, run = run_df,
                 export = export_df, criteria = criteria_df)

    # wrapup agents data
    if(!is.null(ret[[1]]$agents_before)) {
      ret1$agents_before <- nl_run_wrap_result_agents(ret, "agents_before")
    }
    if(!is.null(ret[[1]]$agents_after)) {
      ret1$agents_after <- nl_run_wrap_result_agents(ret, "agents_after")
    }
    if(!is.null(ret[[1]]$agents_step)) {
      ret1$agents_step <- nl_run_wrap_result_agents(ret, "agents_step")
    }
    if(!is.null(ret[[1]]$patches_before)) {
      ret1$patches_before <- nl_run_wrap_result_agents(ret, "patches_before")
    }
    if(!is.null(ret[[1]]$patches_after)) {
      ret1$patches_after <- nl_run_wrap_result_agents(ret, "patches_after")
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

nl_run_init <- function(gui, nl_path, model_path) {
  #Start NetLogo and load model
  nl_options$set("wd", getwd())
  nl_jarname <- list.files(nl_path, pattern = "^netlogo-.*\\.jar$", all.files = TRUE)[1]
  RNetLogo::NLStart(nl.path = nl_path, nl.jarname = nl_jarname, gui=gui)
  RNetLogo::NLLoadModel(model_path)
}

nl_run_end <- function() {
  #Close NetLogo
  RNetLogo::NLQuit()
}

nl_single_run <- function(experiment, parameter_set_id, run_id,
                          param_set, print_progress = FALSE) {
  if(print_progress) {
    cat("Params: ", parameter_set_id, ", Run: ", run_id, "\n", sep = "")
  }
  start_time <- Sys.time()

  # set up parameters and setup NetLogo commands
  nl_single_run_setup(experiment, parameter_set_id, run_id, param_set, print_progress)

  #agents before
  agents_before <- NULL
  if(!is.null(experiment$agents_before)) {
    agents_before <-
      nl_single_agent_report(experiment$agents_before,
                             parameter_set_id,
                             run_id)
  }
  #patches before
  patches_before <- NULL
  if(!is.null(experiment$patches_before)) {
    patches_before <-
      nl_single_patch_report(experiment$patches_before,
                             parameter_set_id,
                             run_id)
  }

  # run
  agents_step <- NULL
  report_step <- NULL

  if(length(experiment$agents_step) > 0 ) {
    #if any step agents reports are defined we have to loop and
    # for each step get all the agents...
    RNetLogo::NLCommand(experiment$run_options$setup_commands)
    iter_id <- 0
    report_step <- NULL
    repeat {
      iter_id <- iter_id + 1
      RNetLogo::NLCommand(experiment$run_options$go_command)
      agents_tmp <-
        nl_single_agent_report(experiment$agents_step,
                               parameter_set_id,
                               run_id = run_id,
                               step_id = iter_id)
      agents_step[[iter_id]] <- agents_tmp
      if(!is.null(experiment$while_condition) &&
         !RNetLogo::NLReport(experiment$while_condition)) {
          break
      }
      if(!is.null(experiment$iterations) &&
         experiment$iterations <= iter_id) {
          break
      }
    }
    agents_step <-
      setNames(
        lapply(seq_along(agents_step[[1]]), function(i) {
          do.call(rbind, lapply(agents_step, function(x) {x[[i]]}))
        }),
        names(agents_step[[1]])
      )
  }
  else if(length(experiment$measures$step) > 0 ) {
    # if any step measures defined - use RNetLogo::NLDoReportWhile
    if(!is.null(experiment$while_condition)) {
      report_step <- RNetLogo::NLDoReportWhile(
        condition = experiment$while_condition,
        command = experiment$run_options$go_command,
        reporter = experiment$measures$step,
        as.data.frame = experiment$measures$as.data.frame,
        df.col.names = names(experiment$measures$step),
        max.minutes = experiment$run_options$max_minutes
      )
    }
    else {
      report_step <- RNetLogo::NLDoReport(
        iterations = experiment$iterations,
        command = experiment$run_options$go_command,
        reporter = experiment$measures$step,
        as.data.frame = experiment$measures$as.data.frame,
        df.col.names = names(experiment$measures$step)
      )
    }
    report_step$step_id <- as.numeric(row.names(report_step))
    report_step$run_id <- run_id
    report_step$param_set_id <- parameter_set_id
    if(!is.null(experiment$measures$step_transform)) {
      report_step <- experiment$measures$step_transform(report_step)
    }
  } else {
    # no step measures - just run the model
    if(!is.null(experiment$while_condition)) {
      RNetLogo::NLDoCommandWhile(
        condition = experiment$while_condition,
        experiment$run_options$go_command,
        max.minutes = experiment$run_options$max_minutes
      )
    } else {
      RNetLogo::NLDoCommand(
        iterations = experiment$iterations,
        experiment$run_options$go_command
      )
    }
    report_step <- NULL
  }
  # compute measures defined per run
  if(length(experiment$measures$run) > 0) {
    report_run <- RNetLogo::NLReport(experiment$measures$run)
    if(experiment$measures$as.data.frame) {
      report_run <- data.frame(report_run)
      names(report_run) <- names(experiment$measures$run)
      report_run$param_set_id <- parameter_set_id
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
                             parameter_set_id,
                             run_id)
  }
  if(!is.null(agents_before)) {
    ret$agents_before <- agents_before
  }
  if(!is.null(agents_step)) {
    ret$agents_step <- agents_step
  }

  # patches
  if(!is.null(experiment$patches_after)) {
    ret$patches_after <-
      nl_single_patch_report(experiment$patches_after,
                             parameter_set_id,
                             run_id)
  }
  if(!is.null(patches_before)) {
    ret$patches_before <- patches_before
  }

  # exports
  if(any(experiment$export_view, experiment$export_world)){
    ret$export <- nl_single_export(experiment, parameter_set_id, run_id)
  }

  # if evaluation criteria are defined
  if(!is.null(experiment$measures$eval_criteria)) {
    criteria_vec <- sapply(experiment$measures$eval_criteria[-1], function(x) with(ret, eval(x)))
    if(mode(criteria_vec) != "numeric") {
      stop("Evaluation criteria is not numeric. It is a (", mode(criteria_vec),")")
    }
    ret$criteria <- as.data.frame(t(unlist(criteria_vec)))
    ret$criteria$param_set_id <- parameter_set_id
    ret$criteria$run_id <- run_id
    ret$step <- NULL
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
#' @param parameter_set_id see nl_single_run
#' @param run_id see nl_single_run
#' @keywords internal
nl_single_run_setup <- function(experiment, parameter_set_id = NULL, run_id = NULL,
                                param_set, print_progress = FALSE) {

  # set random seed
  if(!is.null(experiment$run_options$random_seed)) {
    rseed <- experiment$run_options$random_seed
    if(length(experiment$run_options$random_seed) > 0) {
      rseed <- rseed[min(run_id,length(rseed))]
    }
    RNetLogo::NLCommand("random-seed", rseed)
  }

  # set world size if specified
  if(!is.null(param_set[["world_size"]])) {
    world_size <- param_set[["world_size"]]
    half_size <-  world_size %/% 2
    RNetLogo::NLCommand(sprintf("resize-world %d %d %d %d",
                                -half_size, half_size, -half_size, half_size))
  }
  # set other parameters
  param_names <- setdiff(names(param_set), nl_special_params)
  for(parameter in param_names) {
    nl_param <- nl_map_parameter(experiment, parameter)
    if(nl_param != "") {
      param_value <- param_set[[parameter]]
      RNetLogo::NLCommand(sprintf("set %s %s", nl_param, param_value))
    }
  }

  # execute setup command(s)
  for(command in experiment$run_options$setup_commands) {
    RNetLogo::NLCommand(command)
  }
}

#' Internal: maps parameter
#'
#' @param experiment Experiment object
#' @param parameter_name Parameter name to map
#' @return NetLogo variable name
nl_map_parameter <- function(experiment, parameter_name) {
  nl_param <- experiment$mapping[parameter_name]
  if(is.null(nl_param) || is.na(nl_param)) {
    nl_param <- parameter_name
  }
  nl_param
}

#' Export view and/or world for individual run
#'
#' @param experiment experiment object
#' @param experiment experiment object
#' @keywords internal
nl_single_export <- function(experiment, param_set_id = NA, run_id = 1) {


  file_path <- nl_export_path()
  if(is.null(file_path)) {
    file_path <- nl_options$get("wd")
    file_path <- file.path(file_path, "export")
    if(!dir.exists(file_path)) dir.create(file_path)
  }
  if(is.null(file_path)) file_path <- ""
  ret <- data.frame(param_set_id = param_set_id,
                    run_id = run_id,
                    view = NA,
                    world = NA)
  if(experiment$export_view) {
    fhash <- digest::digest(experiment, algo = "murmur32")

    filename <- sprintf("view_%s_%d_%d.png",
                        fhash,
                        ifelse(is.null(param_set_id), 1, param_set_id),
                        ifelse(is.null(run_id), 1, run_id))
    view_filename <- file.path(file_path, filename)
    RNetLogo::NLCommand(sprintf('export-view "%s"', view_filename))
    ret$view <- view_filename
  }

  if(experiment$export_world) {
    filename <- sprintf("world_%d_%d.csv",
                        ifelse(is.null(param_set_id), 1, param_set_id),
                        ifelse(is.null(run_id), 1, run_id))
    world_filename <- file.path(file_path, filename)
    RNetLogo::NLCommand(sprintf('export-world "%s"', world_filename))
    ret$world <- world_filename
  }
  ret
}

nl_single_agent_report <- function(agent_report,
                                   parameter_set_id = NA,
                                   run_id = 1,
                                   step_id = NULL) {
  lapply(agent_report, function(x) {
    reporters <- sprintf("map [x -> [%s] of x ] sort %s", x$vars, x$agents)
    nlogo_ret <- RNetLogo::NLReport(reporters)
    df1 <- data.frame(nlogo_ret, stringsAsFactors = FALSE)
    names(df1) <- x$vars
    if(!is.null(names(x$vars))) names(df1) <- names(x$vars)
    df1$run_id <- run_id
    df1$param_set_id <- parameter_set_id
    if(!is.null(step_id)) df1$step_id = step_id
    df1
  })
}

nl_single_patch_report <- function(patch_report,
                                   parameter_set_id = NA,
                                   run_id = 1) {

  lapply(patch_report, function(x) {
    reporters <- sprintf("map [x -> [%s] of x ] sort %s", x$vars, x$patches)
    nlogo_ret <- RNetLogo::NLReport(reporters)
    df1 <- data.frame(nlogo_ret, stringsAsFactors = FALSE)
    names(df1) <- x$vars
    if(!is.null(names(x$vars))) names(df1) <- names(x$vars)
    df1$run_id <- run_id
    df1$param_set_id <- parameter_set_id
    df1
  })
}
