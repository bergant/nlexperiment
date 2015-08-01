
#' Import sliders from NetLogo model file
#'
#' Reads NetLogo model file and parses slider section
#'
#' @details Imports parameter names and ranges from sliders defined in
#'   NetLogo model file.
#'   Based on information from
#'     https://github.com/NetLogo/NetLogo/wiki/Model-file-format
#'     https://github.com/NetLogo/NetLogo/wiki/Widget-Format
#'
#' @param experiment NetLogo experiment object
#' @param max_values Maximum values per parameter
#' @return A list with slider data, suggested parameter sets and mapping
#' @examples
#' experiment <- nl_experiment(
#'   model_file =
#'     system.file("netlogo_models/SM2_Hoopoes.nlogo", package = "nlexperiment"),
#'   iterations = 20
#' )
#'
#' # import sliders
#' sliders <- nl_import_sliders(experiment)
#'
#' # set experiment parameter sets
#' experiment <- nl_set_param_values(
#'   experiment,
#'   param_values = sliders$param_values,
#'   mapping = sliders$mapping
#' )
#' @export
nl_import_sliders <- function(experiment, max_values = 20) {

  nlogo <- readLines(experiment$model_file)
  nlogo <- paste(nlogo, collapse = "\n")
  nlogo <- strsplit(nlogo, split = "@#\\$#@#\\$#@")

  interface <- nlogo[[1]][2]
  widgets <- strsplit(interface, "\n\n")[[1]]
  sliders <- widgets[substring(widgets, 1, 6) == "SLIDER"]

  slider_values <-
    do.call(rbind,
            lapply( strsplit(sliders, "\n"), function(x) {
              x <- x[-1]
              data.frame(
                name = x[6], from = as.numeric(x[7]), to = as.numeric(x[8]),
                step = as.numeric(x[10]), def = as.numeric(x[9]),
                stringsAsFactors = FALSE)
            })
    )

  param_values <-
    lapply(seq_len( nrow(slider_values)), function(i){
      x <- slider_values[i,]
      s1 <- seq(from = x$from, to = x$to, by = x$step )
      if(length(s1) > max_values) {
        s1 <- pretty(c(x$from, x$to), max_values)
      }
      s1
    })
  names(param_values) <- gsub("-", "_", slider_values$name)

  mapping <- setNames(slider_values$name, names(param_values))

  list(
    sliders = slider_values,
    param_values = param_values,
    mapping = mapping)
}

