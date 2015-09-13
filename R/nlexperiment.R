#' nlexperiment: NetLogo experiments
#'
#' Exploration of NetLogo (Wilensky 1999) agent based models.
#'
#' @details
#' A tool for NetLogo experiment definition,
#'   exploring simulation results and model optimization.
#'   Makes it easy to turn the cycle of experiment definition,
#'   data analysis, visualisations and
#'   parameter fitting into readable and reproducible documents.
#'
#' RNetLogo package (Thiele 2014) is used as an interface to NetLogo environment.
#'
#' Functions in \bold{nlexperiment} assume the following steps:
#' \itemize{
#' \item Define NetLogo experiment object with parameter sets,
#'    measures and simulation options
#'    (see \code{\link{nl_experiment}} function).
#' \item Run experiment (see \code{\link{nl_run}}).
#'   The result of running an experiment keeps original
#'   experiment definition
#'   along with the simulation results and makes the process of model analysis
#'   more concise and reproducible.
#'   To run the simulation in parallel working processes
#'   use the \code{parallel} attribute in \code{nl_run} function.
#' \item Analyse and present results of simulation(s).
#'   See \code{\link{nl_get_result}} for getting different data from
#'   the result and
#'   \code{\link{nl_show_step}},
#'   \code{\link{nl_show_patches}} for pre-defined plots.
#' \item When additional questions pop out, changes to
#'   experiment will be needed.
#'   Refine the original definition of the experiment by
#'   changing only parameter sets (\code{\link{nl_set_param_values}}),
#'   set different measures (\code{\link{nl_set_measures}}) or set other simulation
#'   options (\code{\link{nl_set_run_options}}).
#' }
#'
#' @docType package
#' @name nlexperiment-package
#' @aliases nlexperiment
#' @aliases nlexperiment-package
#' @references
#'
#'   Wilensky, U. (1999) NetLogo. \url{http://ccl.northwestern.edu/netlogo/}. Center for Connected Learning and Computer-Based Modeling, Northwestern University. Evanston, IL.
#'
#'   Thiele, J. (2014) R Marries NetLogo: Introduction to the RNetLogo Package. Journal of Statistical Software 58(2) 1-41. \url{http://www.jstatsoft.org/v58/i02/}
#'
#'   The ideas and principles of NetLogo experiment definition is taken from
#'   the NetLogo's Behavior Space tool
#'   \url{http://ccl.northwestern.edu/netlogo/docs/behaviorspace.html}
#'   and BehaviorSearch tool \url{http://www.behaviorsearch.org/}
#'
#' @examples
#' \dontrun{
#' # Set the path to your NetLogo installation
#' nl_netlogo_path("c:/Program Files (x86)/NetLogo 5.1.0/")
#'
#' # Create NetLogo experiment of Net Logo Fire model
#' experiment <- nl_experiment(
#'   model_file = "models/Sample Models/Earth Science/Fire.nlogo",
#'   while_condition = "any? turtles",
#'   repetitions = 10,
#'   run_measures = measures(
#'     percent_burned = "(burned-trees / initial-trees) * 100",
#'     progress = "max [pxcor] of patches with [pcolor > 0 and pcolor < 55]"
#'   ),
#'   param_values = list(
#'     density = seq(from = 55, to = 62, by = 1)
#'   )
#' )
#'
#' # Run the experiment using multi-core processing
#' result <- nl_run(experiment, parallel = TRUE)
#'
#' # Get observations data frame
#' dat <- nl_get_run_result(result)
#'
#' # plot percent burned by density
#' library(ggplot2)
#' ggplot(dat, mapping = aes(x = factor(density), y = percent_burned) ) +
#'   geom_violin()
#' }
NULL

# package global variables
nl_experiment_class <- "nl_experiment"
nl_result_class <- "nl_result"
nl_special_params <- c("world_size")


#' Get and set netlogo path
#'
#' @param nl_path An absolute path to your NetLogo installation
#'   On Windows, for example, something like "C:/Program Files/NetLogo 5.1.0".
#' @export
nl_netlogo_path <- function(nl_path = NULL) {
  if(missing(nl_path)) {
    getOption("nlexperiment.netlogo_path")
  } else {
    invisible(options(nlexperiment.netlogo_path = nl_path))
  }
}

#' Get and set export path
#'
#' @param export_path target folder to export files
#' @details Setting export path is optional. If not set, running experiments
#'   with export options (view images and worlds) will create "export"
#'   folder in working directory.
#'   Option is defined per session.
#' @export
nl_export_path <- function(export_path = NULL) {
  if(missing(export_path)) {
    getOption("nlexperiment.export_path")
  } else {
    invisible(options(nlexperiment.export_path = export_path))
  }
}

#' Create NetLogo experiment object
#'
#' Use this function to create NetLogo experiment object.
#'
#' @param model_file An absolute path to your NetLogo model file (.nlogo)
#' @param iterations Number of iterations to run.
#'    Alternatively define while_condition to stop simulation.
#' @param while_condition A string with a NetLogo conditional reporter.
#'     (for example: "ticks < 100")
#' @param repetitions How many times to run the model with the same parameters.
#'   It is set to 1 by default. Result data sets will include run_id as
#'   additional variable to identify the specific runs. To change repetitions
#'   of existing experiment object use \code{\link{nl_set_run_options}}
#' @param random_seed If defined, random seed will be set for each run.
#'   Note: using random seed and repetitions > 1 does not make sense.
#' @param step_measures Measures per each simulation step in a named character
#'   vector. Use measures() function to construct measures in right format.
#'   To change step measures
#'   of existing experiment object use \code{\link{nl_set_measures}}
#' @param run_measures Measures per each simulation run in a named character
#'   vector. Use measures() function to construct measures in right format.
#'   To change run measures
#'   of existing experiment object use \code{\link{nl_set_measures}}
#' @param param_values A data.frame with parameter values or
#'   a list of values to be expanded to all combinations of values
#' @param mapping Mapping between R and NetLogo parameters
#'   in named character vector.
#'   For example: c(diffusion_rate = "diffusion-rate", population = "population")
#' @param agents_after Agents reporters see \code{\link{nl_set_agent_reports}}
#' @param agents_step Agents reporters see \code{\link{nl_set_agent_reports}}
#' @param patches_after Patches reporters see \code{\link{nl_set_agent_reports}}
#' @param export_view If set to TRUE, the views will be exported to
#'   a png image files for each run (when running the experiment)
#' @param export_world If set to TRUE, the world will be exported to
#'   a csv file for each run
#' @param setup_commands NetLogo command strings to execute to setup the model
#' @param go_command NetLogo command string to execute the step in the model
#' @param eval_criteria A criteria calculation expressions.
#'   May use \code{step} or \code{run} data frames to calculate criteria.
#'   Elements from \code{step} should be aggregated.
#'   Must return named numeric vector.
#' @param eval_aggregate_fun Aggregation function
#'   (used to aggregate criteria values when repetitions > 1)
#' @param eval_mutate Add criteria based on aggregated values
#' @param data_handler Function to handle observations. If handler is defined
#'   the observations will not be stored in result elements when running
#'   the experiment with `nl_run` function.
#' @examples
#' experiment <- nl_experiment(
#'   model_file = "models/Sample Models/Earth Science/Fire.nlogo",
#'   while_condition = "any? turtles",
#'   repetitions = 20,
#'   run_measures = measures(
#'     percent_burned = "(burned-trees / initial-trees) * 100",
#'     progress = "max [pxcor] of patches with [pcolor > 0 and pcolor < 55]"
#'   ),
#'   param_values = list(
#'     density = seq(from = 55, to = 62, by = 1)
#'   )
#' )
#' @return NetLogo experiment object
#' @seealso To run experiment use \code{\link{nl_run}}.
#'   To change existing
#'   experiment object see \code{\link{nl_set_measures}},
#'   \code{\link{nl_set_run_options}} and
#'   \code{\link{nl_set_param_values}}.
#' @export
nl_experiment <- function(model_file,
                          iterations = NULL,
                          while_condition = NULL,
                          repetitions = 1,
                          random_seed = NULL,
                          step_measures = NULL,
                          run_measures = NULL,
                          mapping = NULL,
                          param_values = NULL,
                          agents_after = NULL,
                          agents_step = NULL,
                          patches_after = NULL,
                          export_view = FALSE,
                          export_world = FALSE,
                          setup_commands = "setup",
                          go_command = "go",
                          eval_criteria = NULL,
                          eval_aggregate_fun = NULL,
                          eval_mutate = NULL,
                          data_handler = NULL
                          ) {

  # NetLogo model library exemption
  if(length(model_file) != 1)
    stop("model_file must be a character string")

  if(substring(model_file, 1, 21) == "models/Sample Models/") {
    model_file <- file.path(nl_netlogo_path(), model_file)
  }

  experiment <- list(
    model_file = model_file,
    iterations = iterations,
    while_condition = while_condition,
    export_view = export_view,
    export_world = export_world
  )
  class(experiment) <- c(nl_experiment_class, class(experiment))

  # set default run options (1000 iterations)
  experiment <- nl_set_run_options(experiment,
                                   repetitions = repetitions,
                                   random_seed = random_seed,
                                   setup_commands = setup_commands,
                                   go_command = go_command,
                                   data_handler = data_handler)
  # set measures and evaluation criteria
  experiment <- nl_set_measures(experiment,
                                step = step_measures,
                                run = run_measures,
                                eval_criteria = eval_criteria,
                                eval_aggregate_fun = eval_aggregate_fun,
                                eval_mutate = eval_mutate)
  # set agent reports
  experiment <- nl_set_agent_reports(experiment,
                                     agents_after = agents_after,
                                     agents_step = agents_step,
                                     patches_after = patches_after)

  # set parameter sets
  experiment <- nl_set_param_values(experiment,
                                    param_values = param_values,
                                    mapping = mapping)

  return(experiment)
}

#' Convert measure list to named vector
#'
#' @param ... Named character values
#' @examples
#' experiment <- nl_experiment(
#'   model_file = "models/Sample Models/Earth Science/Fire.nlogo",
#'   while_condition = "any? turtles",
#'   repetitions = 20,
#'   run_measures = measures(
#'     percent_burned = "(burned-trees / initial-trees) * 100",
#'     progress = "max [pxcor] of patches with [pcolor > 0 and pcolor < 55]"
#'   ),
#'   param_values = list(
#'     density = seq(from = 55, to = 62, by = 1)
#'   )
#' )
#' @export
#' @keywords internal
measures <- function(...) {
  unlist(list(...))
}

#' Criteria expression from a list of expressions
#'
#' Used in evaluate element in \code{\link{nl_experiment}} or
#'   \code{\link{nl_set_measures}}
#'
#' @details Must evaluate to a numeric
#' @param ... expressions
#' @export
#' @keywords internal
criteria <- function(...) {
  substitute(list(...))
}


#' Create an agent set reporter
#'
#' Create an agent set reporter to set agent reporters in `nl_experiment` or
#'   `nl_set_agent_reports`
#'
#' @param vars A string or vector/list of strings with the variable names of the agent(s).
#' @param agentset A string specifying the agent or agentset to be queried.
#' @examples
#' experiment <- nl_experiment(
#'   model_file = "models/Sample models/Networks/Preferential attachment.nlogo",
#'   iterations = 30,
#'   export_view = TRUE,
#'   agents_after = list(
#'     vertices = agent_set(c("who", "xcor", "ycor"), "turtles"),
#'     edges = agent_set(vars = c(e1 = "[who] of end1", e2 ="[who] of end2"), agents = "links")
#'   )
#' )
#' @export
#' @keywords internal
agent_set <- function(vars, agents) {
  if(is.null(names(vars))) names(vars) <- vars
  names(vars) <- ifelse( names(vars) == "", vars, names(vars))
  list(vars = vars, agents = agents)
}

#' Create a patch set reporter
#'
#' Create a patch set reporter to set patch reporters in `nl_experiment` or
#'   `nl_set_agent_reports`
#'
#' @param vars A string or vector/list of strings with the variable names of the agent(s).
#' @param patches A string specifying the patches to be queried.
#' @export
#' @keywords internal
patch_set <- function(vars = c("pxcor", "pycor", "pcolor"),
                      patches = "patches") {
  list(vars = vars, patches = patches)
}

#' Set run options of a NetLogo experiment object
#'
#' You can set basic run options when creating experiment object
#' with \code{\link{nl_experiment}}. To change these or add
#' additional options use \code{nl_set_run_options}
#'
#' @param experiment NetLogo experiment object from nl_experiment() function
#' @param random_seed Random seed
#' @param repetitions Number of repetitions (when random seed is not defined)
#' @param max_minutes If max.minutes > 0 the execution stops after the
#'     defined number of minutes (with an error and no return value)
#'     Default value is 10.
#' @param setup_commands NetLogo command strings to execute to setup the model
#' @param go_command NetLogo command string to execute the step in the model
#' @param data_handler Function to handle observations. If handler is defined
#'   the observations will not be stored in result elements when running
#'   the experiment with `nl_run` function.
#' @return NetLogo experiment object
#' @examples
#'
#' experiment <- nl_experiment(
#'   model_file = "my_model.nlogo",
#'   while_condition = "any? turtles"
#' )
#'
#' experiment <- nl_set_run_options(
#'   experiment,
#'   repetitions = 3,
#'   setup_commands = c("setup", "change_something")
#' )
#'
#' @export
nl_set_run_options <- function(
  experiment,
  random_seed = NULL,
  repetitions = 1,
  max_minutes = 10,
  setup_commands = "setup",
  go_command = "go",
  data_handler = NULL
) {

  if(!inherits(experiment, nl_experiment_class))
    stop("Not a NetLogo experiment object")


  experiment$run_options <- list(
    random_seed = random_seed,
    repetitions = repetitions,
    max_minutes = max_minutes,
    setup_commands = setup_commands,
    go_command = go_command,
    data_handler = data_handler
  )
  experiment
}


#' Set or change measures of existing NetLogo experiment
#'
#' @param experiment NetLogo experiment object
#' @param step NetLogo reporters for each step (reported at every tick).
#'   A list of named character vectors. Use \code{\link{measures}} function to get
#'   the correct structure.
#' @param run NetLogo reporters for each run (reported at end of run).
#'   A list of named character vectors. Use \code{\link{measures}} function to get
#'   the correct structure.
#' @param eval_criteria A criteria calculation expressions.
#'   May use \code{step} or \code{run} data frames to calculate criteria.
#'   Elements from \code{step} should be aggregated.
#'   Must return named numeric vector.
#' @param eval_aggregate_fun Aggregate criteria.
#'   It makes sense when when repetitions > 1
#' @param eval_mutate Add criteria based on aggregated values
#' @param as.data.frame Reporting in data frame format (TRUE by default)
#' @param step_transform A function to transform data frame result from
#'   step reporters. When simulation has many steps and only summary
#'   data is needed, step_transform can reduce memory requirements to
#'   run experiment.
#' @details Values of experiment measures are NetLogo reporters.
#'   Names of measures will be used in the resulting data frames as
#'   column names.
#' @seealso To create an experiment object use \code{\link{nl_experiment}}
#' @return NetLogo experiment object
#' @export
nl_set_measures <- function(experiment,
                        step = NULL,
                        run = NULL,
                        eval_criteria = NULL,
                        eval_aggregate_fun = NULL,
                        eval_mutate = NULL,
                        as.data.frame = TRUE,
                        step_transform = NULL) {
  if(!inherits(experiment, nl_experiment_class))
    stop("Not a NetLogo experiment object")

  experiment$measures <-
    list(step = step,
         run = run,
         eval_criteria = eval_criteria,
         eval_aggregate_fun = eval_aggregate_fun,
         eval_mutate = eval_mutate,
         as.data.frame = as.data.frame,
         step_transform = step_transform)

  experiment
}

#' Set or change agent reports
#'
#' Set reporting of variable value(s) of one or more agent(s) as a data.frame
#'
#' @param experiment NetLogo experiment object
#' @param agents_before A list of agent reports to be accessed before each run.
#' @param agents_after A list of agent reports to be accessed after each run.
#' @param agents_step A list of agent reports to be accessed per each iteration (step).
#' @param patches_after A list of patches reports to be accessed after each run
#' @param patches_before A list of patches reports to be accessed before each run
#' @seealso To create an experiment object use \code{\link{nl_experiment}}
#' @return NetLogo experiment object
#' @export
nl_set_agent_reports <- function(experiment,
                                 agents_before = NULL,
                                 agents_after = NULL,
                                 agents_step = NULL,
                                 patches_before = NULL,
                                 patches_after = NULL) {
  if(!inherits(experiment, nl_experiment_class))
    stop("Not a NetLogo experiment object")

  if(!missing(agents_before)) {
    experiment$agents_before <- agents_before
  }
  if(!missing(agents_after)) {
    experiment$agents_after <- agents_after
  }
  if(!missing(agents_step)) {
    experiment$agents_step <- agents_step
  }
  if(!missing(patches_before)) {
    experiment$patches_before <- patches_before
  }
  if(!missing(patches_after)) {
    experiment$patches_after <- patches_after
  }
  experiment
}


#' Define parameter sets for NetLogo experiment
#'
#' @param experiment NetLogo experiment object from nl_experiment() function
#' @param param_values A data.frame with parameter values or
#'   a list of values to be expanded to all combinations of values
#' @param mapping Mapping between R and NetLogo parameters
#'   in named character vector.
#'   For example: c(diffusion_rate = "diffusion-rate", population = "population")
#' @return NetLogo experiment object
#' @export
nl_set_param_values <- function(experiment, param_values = NULL, mapping = NULL ) {
  if(!inherits(experiment, nl_experiment_class))
    stop("Not a NetLogo experiment object")

  if(missing(param_values) || is.null(param_values)) {
    param_sets <- data.frame()
  } else if(inherits(param_values, "data.frame")) {
    param_sets <- param_values
  } else if(inherits(param_values, "list")) {
    param_sets <- expand.grid(param_values)
  } else {
    stop("Attribute param_values should be a data frame or a list")
  }

  experiment$param_sets <- param_sets

  if(!missing(mapping)) {
    if(mode(mapping) == "function") {
      experiment$mapping <- mapping(experiment$param_sets)
    } else {
      experiment$mapping <- mapping
    }
  }
  experiment
}


#' Print NetLogo experiment object
#'
#' @param x NetLogo experiment object
#' @param ... further arguments passed to or from other methods.
#' @export
print.nl_experiment <- function(x, ...) {
  if(!inherits(x, nl_experiment_class))
    stop("Not a NetLogo experiment object")

  with(x, {
    cat("NetLogo experiment object -", basename(model_file),"\n")
    cat("  Setup procedures: ")
    cat(run_options$setup_commands, sep = " ")
    cat("\n")
    cat("  Go command:", run_options$go_command, "\n")
    if(!is.null(while_condition)) cat("  Run condition:", while_condition, "\n")
    if(!is.null(iterations)) cat("  Iterations:", iterations, "steps\n")
    if(run_options$repetitions > 1) cat("  Repetitions:", run_options$repetitions, "simulations\n")
    if(!is.null(run_options$random_seed)) cat("  Random seed", run_options$random_seed, "\n")


    cat("Parameter sets: ")
    if(!is.null(param_sets) && nrow(param_sets) > 0) {
      cat("(", nrow(param_sets), ")\n")
      cat(
        paste("   ",capture.output(
          nl_get_param_range(x, diff_only = FALSE, as.data.frame = TRUE))[-1],
                collapse = "\n" )
      )

    } else {
      cat("No parameters")
    }
    cat("\n")


    cat("Measures and criteria:\n")
    if(length(names(measures$step))>0) {
      cat("  Step measures: ", paste(names(measures$step), collapse = ", "), "\n")
    }
    if(length(names(measures$run))>0) {
      cat("  Run measures: ", names(measures$run), "\n")
    }
    if(!is.null(measures$eval_criteria)) {
      cnames <- names(measures$eval_criteria[-1])
      if(!is.null(measures$eval_mutate)) {
        cnames <- c(cnames, names(measures$eval_mutate[-1]))
      }
      cat("  Criteria: ", paste(cnames, collapse = ", "), "\n")
    }
  })
}



#' Get ranges of experiment parameter sets
#'
#' Upper and lower value for each parameter in experiment parameter sets
#'
#' @param experiment NetLogo experiment object
#' @param diff_only Uses only non-constant parameters
#' @param as.data.frame Return in a data frame
#' @return A list with lower and upper values for all parameters in
#'   experiment parameter set.
#'   When as.data.frame is specified
#'   a data frame with lower and upper columns.
#' @export
nl_get_param_range <- function(experiment, diff_only = TRUE, as.data.frame = FALSE) {
  min_values <- sapply(experiment$param_sets, min)
  max_values <- sapply(experiment$param_sets, max)

  ret <- list(
    lower = min_values[min_values != max_values | !diff_only],
    upper = max_values[min_values != max_values | !diff_only]
  )
  if(as.data.frame) {
    ret <- data.frame(ret)
  }
  ret
}

#' Create parameter sets with latin hypercube sampling
#'
#' Parameter sets are created with \code{lhs} function from \pkg{tgp} package
#'
#' @param n Number of parameter sets
#' @param ... Named list of parameter ranges (numeric vectors)
#' @return A data frame with parameter value sets
#' @export
#' @examples
#' experiment <- nl_experiment(
#'   model_file = "models/Sample Models/Biology/Flocking.nlogo",
#'   setup_commands = c("setup", "repeat 100 [go]"),
#'   iterations = 5,
#'
#'   param_values = nl_param_lhs(
#'     n = 100,                           # create 100 parameter value sets
#'     world_size = 50,
#'     population = 80,
#'     vision = 6,
#'     min_separation = c(0, 4),
#'     max_align_turn = c(0, 20)
#'   ),
#'   mapping = c(
#'     min_separation = "minimum-separation",
#'     max_align_turn = "max-align-turn"),
#'
#'   step_measures = measures(
#'     converged = "1 -
#'     (standard-deviation [dx] of turtles +
#'     standard-deviation [dy] of turtles) / 2",
#'     mean_crowding =
#'       "mean [count flockmates + 1] of turtles"
#'   ),
#'   eval_criteria = criteria(
#'     c_converged = mean(step$converged),
#'     c_mcrowding = mean(step$mean_crowding)
#'   ),
#'
#'   repetitions = 10,                        # repeat simulations 10 times
#'   random_seed = 1:10,
#'
#'   eval_aggregate_fun = mean                # aggregate over repetitions
#' )
nl_param_lhs <- function(n, ...) {

  if( !requireNamespace("tgp", quietly = TRUE)) {
    stop("tgp package needed for nl_param_lhs function to work. Please install it.",
         call. = FALSE)
  }

  p_list <- list(...)
  params <- lapply(p_list, range)
  params <- tgp::lhs(n = n, rect = t(data.frame(params)))
  setNames(as.data.frame(params), names(p_list))
}

#' Create parameter sets with "one-at-a-time" (OAT) approach
#'
#' @param n Number of parameter sets per parameter
#' @param ... Named list of parameter ranges (numeric vectors)
#'   Minimum and maximum values are used as a range and
#'   median as the default value. Parameters with only 1 value
#'   are treated as constants.
#' @return A data frame with parameter value sets
#' @seealso See also \code{\link{nl_param_lhs}} for latin cube and
#'   \code{\link{nl_param_fast}} for FAST parameter sampling.
#' @export
#' @examples
#'
#' # create 5 values for every parameter:
#' nl_param_oat(n = 5, P1 = c(1, 4, 10), P2 = c(4, 11, 20))
#'
#' # using constant parameters:
#' nl_param_oat(n = 5, P1 = c(1, 4, 10), P2 = c(4, 11, 20), P3 = 6)
#'
#' # define NetLogo experiment with OAT design:
#' experiment <- nl_experiment(
#'   model_file = "models/Sample Models/Biology/Flocking.nlogo",
#'   setup_commands = c("setup", "repeat 100 [go]"),
#'   iterations = 5,
#'
#'   param_values = nl_param_oat(
#'     n = 25,                           # create 25 value sets per parameter
#'     max_align_turn = c(0, 5, 20),
#'     max_cohere_turn = c(0, 3, 20),
#'     max_separate_turn = c(0, 1.5, 20),
#'     vision = c(1, 3, 10),
#'     minimum_separation = c(0, 3, 10),
#'     .dummy = c(0,0.5,1),
#'     world_size = 50,
#'     population = 80
#'   ),
#'   mapping = nl_default_mapping,
#'
#'   step_measures = measures(
#'     converged = "1 -
#'     (standard-deviation [dx] of turtles +
#'     standard-deviation [dy] of turtles) / 2",
#'     mean_crowding =
#'       "mean [count flockmates + 1] of turtles"
#'   ),
#'   eval_criteria = criteria(
#'     c_converged = mean(step$converged),
#'     c_mcrowding = mean(step$mean_crowding)
#'   ),
#'
#'   repetitions = 10,                        # repeat simulations 10 times
#'   random_seed = 1:10
#'
#' )
nl_param_oat <- function(n, ...) {

  if(mode(n) != "numeric") {
    stop("Parameter n has to be a number.")
  }

  param_values <- list(...)

  if(length(param_values) == 1 &&
     inherits(param_values[[1]], c("list", "data.frame"))) {
    param_values <- param_values[[1]]
  }

  range_pv <- param_values[sapply(param_values, function(x) min(x) < max(x))]
  range_pv <- lapply(range_pv, function(x) {
    rand_x <- (order(runif(n)) - 1 + runif(n))/n
    p_start <- min(x)
    p_width <- max(x) - p_start
    rand_x * p_width + p_start
  })

  param_sets <-
    lapply(names(range_pv), function(r) {
      param_values2 <- lapply(param_values, function(x) median(x))
      param_values2[[r]] <- range_pv[[r]]
      expand.grid(param_values2)
    })

  do.call(rbind, param_sets)
}

#' Generate a parameter value sets for the FAST method
#'
#' Uses \code{\link[fast]{fast_parameters}} from \pkg{fast} package to create
#' parameter sets for Fourier Amplitute Sensitivity Test (FAST).
#' @details
#' Uses only parameters with min != max values to create parameter sets.
#' Adds dummy variable.
#' @param ... Named list of parameter ranges (numeric vectors)
#' @return A data frame with parameter value sets.
#' @seealso
#'   Use \code{\link{nl_get_fast_sensitivity}} to get sensitivity data.
#'   See \link[fast]{fast} package documentation for FAST algorithm details.
#'   from the simulation results.
#'   See \code{\link{nl_param_lhs}} for latin hypercube sampling.
#' @examples
#' param_values <- nl_param_fast(
#'   world_size = 50,
#'   population = 80,
#'   max_align_turn = c(1, 5, 20),
#'   max_cohere_turn = c(1, 3, 20),
#'   max_separate_turn = c(1, 1.5, 20),
#'   vision = c(1, 3, 10),
#'   minimum_separation = c(1, 3, 10)
#' )
#' @export
nl_param_fast <- function(...) {

  if( !requireNamespace("fast", quietly = TRUE)) {
    stop("fast package needed for nl_param_fast. Please install it",
         call. = FALSE)
  }

  param_values <- list(...)
  if(length(param_values) == 1 && inherits(param_values[[1]], "list"))
    param_values <- param_values[[1]]

  ranges_min <- sapply(param_values, min)
  ranges_max <- sapply(param_values, max)
  ranges_const <- ranges_min == ranges_max

  # create parameter with fast
  fast_param_values <- fast::fast_parameters(
    minimum = ranges_min[!ranges_const],
    maximum = ranges_max[!ranges_const])

  names(fast_param_values) <- names(param_values)[!ranges_const]

  for(const_param in names(param_values[ranges_const])) {
    fast_param_values[[const_param]] <- param_values[[const_param]][1]
  }
  fast_param_values
}

#' Default mapping from R names to NetLogo variables
#'
#' Creates mapping with simple rule:
#' changes every character
#'   \code{_.} to \code{?} and
#'   \code{_} to \code{-}.
#'
#' @param param_values Parameter values in list or data frame
#' @return Named vector with default mapping.
#'   Use as function argument in \code{\link{nl_experiment}} mapping.
#' @examples
#'
#' param_values = list(
#'   world_size = 50,
#'   population = 80,
#'   max_align_turn = c(1, 5, 20),
#'   max_cohere_turn = c(1, 3, 20),
#'   max_separate_turn = c(1, 1.5, 20),
#'   vision = c(1, 3, 10),
#'   minimum_separation = c(1, 3, 10),
#'   .dummy = c(1:0)
#' )
#'
#' nl_default_mapping(param_values)
#'
#' # Define experiment mapping with a function instead of named vector:
#' experiment <- nl_experiment(
#'   model_file = "models/Sample Models/Biology/Flocking.nlogo",
#'
#'   param_values = list(
#'     world_size = 50,
#'     population = 80,
#'     max_align_turn = c(1, 5, 20),
#'     max_cohere_turn = c(1, 3, 20),
#'     max_separate_turn = c(1, 1.5, 20),
#'     vision = c(1, 3, 10),
#'     minimum_separation = c(1, 3, 10),
#'     .dummy = c(1:0)
#'   ),
#'   mapping = nl_default_mapping
#' )
#'
#' # check experiment parameter names mapping
#' cbind(experiment$mapping)
#'
#' @export
nl_default_mapping <- function(param_values) {
  mapping <- names(param_values)
  mapping <- gsub("_\\.", "?", mapping)
  mapping <- gsub("_", "-", mapping)
  mapping <- ifelse(substr(mapping, 1, 1) == ".", "" , mapping)
  setNames(mapping, names(param_values))
}

#' Plots parameters with scatter plots
#'
#' @param experiment Experiment object
#' @param cex Parameter passed to pairs function
#' @param col Parameter passed to pairs function
#' @param lower.panel Parameter passed to pairs function
#' @param ... Parameters passed to pairs function
#' @export
nl_show_params <- function(experiment, cex = 0.7, col = "#000000CC",
                           lower.panel = NULL, ...) {

  variable_params <-
    sapply(experiment$param_sets, function(x) min(x) != max(x))

  pairs(experiment$param_sets[variable_params],
        cex = 0.7, col = "#000000CC", lower.panel = NULL, ...)
}

