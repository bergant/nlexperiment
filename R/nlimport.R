
#' Import sliders from NetLogo model file
#'
#' Reads NetLogo model file and parses slider section
#'
#' @details Imports parameter names and ranges from sliders defined in
#'   NetLogo model file.
#'   Based on information from
#'     https://github.com/NetLogo/NetLogo/wiki/Model-file-format
#'     and
#'     https://github.com/NetLogo/NetLogo/wiki/Widget-Format
#'
#' @param experiment NetLogo experiment object
#' @param max_values Maximum values per parameter
#' @return A list with slider data, suggested parameter sets and mapping
#' @export
nl_import_sliders <- function(experiment, max_values = 20) {

  nlogo <- readLines(experiment$model_file)
  nlogo <- paste(nlogo, collapse = "\n")
  nlogo <- strsplit(nlogo, split = "@#\\$#@#\\$#@")

  interface <- nlogo[[1]][2]
  widgets <- strsplit(interface, "\n\n")[[1]]
  sliders <- widgets[substring(widgets, 1, 6) == "SLIDER"]
  switches <- widgets[substring(widgets, 1, 6) == "SWITCH"]

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

  switches_values <-
    do.call(rbind,
            lapply( strsplit(switches, "\n"), function(x) {
              x <- x[-1]
              data.frame(
                name = x[6],
                def = 1 - as.numeric(x[7]),
                stringsAsFactors = FALSE)
            })
    )

  list(
    sliders = slider_values,
    param_values = param_values,
    mapping = mapping,
    switches = switches_values
  )
}

