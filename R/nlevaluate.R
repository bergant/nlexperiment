
#' Evaluate experiment with specific parameters
#'
#' @description Function \code{nl_eval_run} runs experiment as
#'   with \code{\link{nl_run}} but requires started NetLogo instance
#'   with loaded model.
#' @param param_set parameter set (a list of parameters with values)
#' @param experiment NetLogo experiment object (see \code{\link{nl_experiment}})
#' @param criteria Which experiment evaluation criteria to be returned
#' @param print_progress print evaluation progress
#' @param call_back A call-back function for tracing result in optimization
#'   processes
#' @param cluster Required for parallel execution (nl_eval_init returns
#'   cluster object)
#' @param param_names parameter names for parameter set
#' @details Use when parameters depend on previous evaluation (like in
#'   parameter fitting / callibration / optimization methods).
#'   It can use the same experiment object as \code{nl_run} function
#'   where evaluation criteria should be defined
#'   (see \code{\link{nl_experiment}} or \code{\link{nl_set_measures}}).
#' @export
nl_eval_run <- function(param_set, experiment, criteria = NULL,
                        print_progress = FALSE, call_back = NULL,
                        parallel = FALSE, cluster = NULL,
                        param_names = NULL) {

  if(!inherits(experiment, nl_experiment_class))
    stop("Not a NetLogo experiment object")

#   if(length(param_set) != ncol(experiment$param_sets)) {
#     stop("Number of parameters not equal to number of ",
#          "parameters in experiment object: ",
#          length(param_set) ," / ",
#          ncol(experiment$param_sets))
#   }
  start_time <- Sys.time()
  if(!inherits(param_set, "data.frame")) {
    if(!inherits(param_set, "list")) {
      if(is.null(names(param_set))) {
        if(missing(param_names)) param_names <- names(experiment$param_sets)
        names(param_set) <- param_names
      }
      param_set <- as.data.frame(as.list(param_set))
    } else {
      param_set <- as.data.frame(param_set)
    }
  }
  param_set <- cbind(
    param_set,
    experiment$param_sets[1, !names(experiment$param_sets) %in% names(param_set)])
  run_schedule <- nl_get_schedule(experiment, param_set)
  ret <- nl_run_schedule(experiment, run_schedule = run_schedule,
                         print_progress = print_progress,
                         parallel = parallel,
                         cluster = cluster)
  if(is.null(ret)) {
    stop("Evaluation returned nothing")
  }
  if(!missing(criteria)) {
    ret <- ret$criteria[[criteria]]
  } else {
    ret <- ret$criteria
  }
  if(!missing(call_back) && !is.null(call_back)) {
    call_back(
      cbind(param_set, result = ret))
  }
  invisible(ret)
}

#' @description Function \code{nl_eval_init} starts NetLogo instance and
#'   loads the NetLogo model. When using parallel version it initializes several
#'   processes and returns cluster objects
#' @param parallel If TRUE nl_eval_init returns cluster object which should be
#'   passed to nl_eval_run and nl_eval_close.
#' @param max_cores If not defined all available cores are used.
#' @rdname nl_eval_run
#' @export
nl_eval_init <- function(experiment, parallel = FALSE, max_cores = NULL) {
  if(!parallel) {
    nl_run_init(gui = FALSE, nl_path = nl_netlogo_path(),
                model_path = experiment$model_file)
    return(invisible(NULL))
  }

  #parallel
  processors <- parallel::detectCores()
  if(!missing(max_cores)) {
    processors <- min(processors, max_cores)
  }
  cluster <- parallel::makeCluster(processors)

  parallel::clusterCall(cluster, nl_run_init, gui=FALSE, nl_path = nl_netlogo_path(),
                        model_path = experiment$model_file)
  return(cluster)

}

#' @description Function \code{nl_eval_close} stops NetLogo instance
#' @rdname nl_eval_run
#' @export
nl_eval_close <- function(parallel = FALSE, cluster = NULL) {
  if(!parallel) {
    nl_run_end()
  } else
  {
    parallel::clusterCall(cluster, nl_run_end)
    parallel::stopCluster(cluster)
  }
}

#' Iterations call-back factory
#' @param verbose When TRUE adding new data will print the line
#' @export
nl_eval_tracer <- function(verbose = TRUE) {
  iterations <- data.frame()
  verb <- verbose
  add <- function (x) {
    i <- nrow(iterations) + 1
    x$iter_id <- i
    if(ncol(iterations) > 0 && names(x) != names(iterations)) {
      iterations <<- data.frame()
    }
    iterations <<- rbind(iterations, x)
    if(verb) print(iterations[i,])
  }
  get <- function () {
    iterations
  }
  return(list(add = add, get = get))
}

#' @description Function \code{nl_get_eval_fun} returns a function
#'   wich calls \code{\link{nl_eval_run}} but does not need
#'   additional parameters.
#' @rdname nl_eval_run
#' @export
nl_get_eval_fun <- function(experiment, param_names, parallel = FALSE, cluster = NULL, criteria, call_back = NULL) {
  eval_fun <- function(x) {
    nl_eval_run(x,
                experiment = experiment,
                criteria = criteria,
                call_back = call_back,
                parallel = parallel,
                cluster = cluster,
                param_names = param_names)
  }
  return(eval_fun)
}
