#' nlexperiment: NetLogo experiments
#'
#' Define and run NetLogo (Wilensky 1999) model experiments.
#'
#' @details
#'
#' \link{RNetLogo} package (Thiele 2014) opens countless possibilities
#' by connecting NetLogo to R.
#' But it does require some programming to define and run
#' model experiments.
#' The purpose of \bold{nlexperiment} is to make it simple as possible
#' while keeping complex functionalities as options.
#' User can start with
#' experiment (analogous to NetLogo Behavior Space),
#' run the experiment, explore the results
#' and return to refine the experiment definition.
#'
#' Functions in \bold{nlexperiment} assume the following steps:
#' \itemize{
#' \item Define NetLogo experiment object with parameter space definition,
#'    selected measures and other related simulation options
#'    (see \code{\link{nl_experiment}} function).
#' \item Run experiment (see \code{\link{nl_run}}).
#'   The result of running an experiment keeps original
#'   experiment definition
#'   along with the simulation results and makes the process of model analysis
#'   more concise and reproducible.
#'   To run the simulation in parallel working processes
#'   just use the \code{parallel} attribute in \code{nl_run} function.
#' \item Analyse and present results of simulation(s).
#' \item When additional questions pop out, changes to
#'   experiment will be needed.
#'   Refine the original definition of the experiment by
#'   changing only parameter space (\code{\link{nl_set_param_space}}),
#'   set different measures (\code{\link{nl_set_measures}}) or set other simulation
#'   options (\code{\link{nl_set_run_options}}).
#' }
#'
#' @docType package
#' @name nlexperiment
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
#'
#'   The parallel implementation of \code{nl_run} is based on the RNetLogo vignette
#'   \url{https://cran.r-project.org/web/packages/RNetLogo/vignettes/parallelProcessing.pdf}
#'
#' @examples
#' \dontrun{
#' # Set the path to your NetLogo installation
#' nl_netlogo_path("c:/Program Files (x86)/NetLogo 5.1.0/")
#'
#' # Fire model is included in NetLogo sample models:
#' fire_model <- file.path(nl_netlogo_path(),
#'   "models/Sample Models/Earth Science/Fire.nlogo")
#'
#' # Create NetLogo experiment object
#' experiment <- nl_experiment(
#'   model_file = fire_model,
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
#' # Run the experiment
#' result <- nl_run(experiment)
#'
#' dat <- nl_get_run_result(result)
#'
#' library(ggplot2)
#' # plot percent burned by density
#' ggplot(dat, mapping = aes(x = factor(density), y = percent_burned) ) +
#'   geom_violin() +
#'   geom_jitter(position = position_jitter(width = 0.2), alpha = 0.3)
#' }
NULL

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
#' @details Option is defined per session. When R session is restarded
#'   and nlexperiment loaded, NetLogo path is empty.
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
#' @details Setting export path is optional. If not set, running experiments
#'   with export options (view images and worlds) will create "export"
#'   folder in working directory.
#'   Option is defined per session. When R session is restarded
#'   and nlexperiment loaded, the export path is empty.
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
#' Use this function to create NetLogo experiment object.
#'
#' @param model_file An absolute path to your NetLogo model file (.nlogo)
#' @param max_ticks Number of iterations to run.
#'    This parameter is used when while_condition is not defined.
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
#' @param export_view If set to TRUE, the views will be exported to
#'   a png image files for each run (when running the experiment)
#' @param export_world If set to TRUE, the world will be exported to
#'   a csv file for each run
#' @examples
#' experiment <- nl_experiment(
#'   model_file = "my_model.nlogo",
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
#'   \code{\link{nl_set_param_space}}.
#' @export
nl_experiment <- function(model_file,
                          max_ticks = NULL,
                          while_condition = NULL,
                          repetitions = 1,
                          random_seed = NULL,
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
  experiment <- nl_set_run_options(experiment,
                                   repetitions = repetitions,
                                   random_seed = random_seed)
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
#' @examples
#' experiment <- nl_experiment(
#'   model_file = "my_model.nlogo",
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
#' @param setup_commands NetLogo command strings to execute to setup the model
#' @param go_command NetLogo command string to execute the step in the model
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


#' Set or change measures of existing NetLogo experiment
#'
#' @param experiment NetLogo experiment object
#' @param step NetLogo reporters for each step (reported at every tick).
#'   A list of named character vectors. Use \code{\link{measures}} function to get
#'   the correct structure.
#' @param run NetLogo reporters for each run (reported at end of run).
#'   A list of named character vectors. Use \code{\link{measures}} function to get
#'   the correct structure.
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
    cat("  Setup procedures: ")
    if(length(run_options$setup_commands) > 1) {
      cat("\n")
    }
    cat(run_options$setup_commands, sep = "\n    ")
    cat("  Go command: ", run_options$go_command, "\n")
    if(length(names(measures$step))>0) {
      cat("Step measures: ", names(measures$step), "\n")
    }
    if(length(names(measures$run))>0) {
      cat("Run measures: ", names(measures$run), "\n")
    }
    cat("Parameter space: ")
    if(!is.null(param_space) && nrow(param_space) > 0) {
      cat("\n  Size: ", nrow(param_space))
      cat("\n  Parameters: ", names(param_space))
    } else {
      cat("No parameters")
    }
    cat("\n")
  })
}

