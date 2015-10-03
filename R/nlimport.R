#' Parse NetLogo model file
#'
#' Gets information about widgets (e.g. sliders, monitors, plots) from from NetLogo model file
#'
#' @details Imports attributes from sliders defined in
#'   NetLogo model file.
#'   Based on information from
#'     \url{https://github.com/NetLogo/NetLogo/wiki/Model-file-format}
#'     and
#'     \url{https://github.com/NetLogo/NetLogo/wiki/Widget-Format}
#' @param model_file NetLogo model file
#' @return Returns an object of class \code{nl_model}.
#'   It is a list containing at most the following components:
#'   \item{ view }{a data frame with NetLogo model view attributes}
#'   \item{ sliders }{a data frame with NetLogo model sliders attributes}
#'   \item{ switches }{a data frame with NetLogo model switches attributes}
#'   \item{ monitors }{a data frame with NetLogo model monitors attributes}
#'   \item{ plots }{a data frame with NetLogo model plots attributes}
#' @export
nl_parse_model <- function(model_file) {

  if(missing(model_file)) {
      stop("Define experiment or model file")
  }

  as_num <- function(x) {
    suppressWarnings(as.numeric(x))
  }

  nlogo <- readLines(model_file)
  nlogo <- paste(nlogo, collapse = "\n")
  nlogo <- strsplit(nlogo, split = "@#\\$#@#\\$#@")

  model_descriprion <- nlogo[[1]][3]
  netlogo_version <- trimws(nlogo[[1]][5])
  behavior_space <- trimws(nlogo[[1]][7], which = "left")

  interface <- nlogo[[1]][2]
  widgets <- strsplit(interface, "\n\n")[[1]]
  sliders <- widgets[substring(widgets, 1, 6) == "SLIDER"]
  switches <- widgets[substring(widgets, 1, 6) == "SWITCH"]
  monitors <- widgets[substring(widgets, 1, 7) == "MONITOR"]
  plots <- widgets[substring(widgets, 1, 4) == "PLOT"]
  view <- widgets[substring(widgets, 1, 16) == "\nGRAPHICS-WINDOW"]

  view_values <- {
    view_attrs <- strsplit(view, "\n")[[1]][-(1:2)]
    data.frame(
      patch_size = as_num(view_attrs[7]),  # patchsize
      wrapping_x = as_num(view_attrs[14]), # wrappingAllowedInX
      wrapping_y = as_num(view_attrs[15]), # wrappingAllowedInY
      minPxcor = as_num(view_attrs[17]),   # minPxcor
      maxPxcor = as_num(view_attrs[18]),   # maxPxcor
      minPycor = as_num(view_attrs[19]),   # minPycor
      maxPycor = as_num(view_attrs[20]),   # maxPycor
      tick_label = view_attrs[21],         # tick counter label
      frame_rate = as_num(view_attrs[22]), # frame rate (defaults to 30)
      stringsAsFactors = FALSE
    )
  }

  slider_values <-
    do.call(rbind,
            lapply( strsplit(sliders, "\n"), function(x) {
              x <- x[-1]
              data.frame(
                name = x[6],
                from = as_num(x[7]),
                to = as_num(x[8]),
                step = as_num(x[10]), def = as_num(x[9]),
                stringsAsFactors = FALSE)
            })
    )


  switches_values <-
    do.call(rbind,
            lapply( strsplit(switches, "\n"), function(x) {
              x <- x[-1]
              data.frame(
                name = x[6],
                def = 1 - as_num(x[7]),
                stringsAsFactors = FALSE)
            })
    )

  monitors_values <-
    do.call(rbind,
            lapply( strsplit(monitors, "\n"), function(x) {
              x <- x[-1]
              data.frame(
                name = ifelse(x[5] == "NIL", x[6], x[5]),
                expression = x[6],
                stringsAsFactors = FALSE)
            })
    )

  plots_values <-
    do.call(rbind,
            lapply( strsplit(plots, "\n"), function(x) {
              x <- x[-1]
              ret <-
                data.frame(
                  name = x[5],
                  xaxis = x[6],
                  yaxis = x[7],
                  stringsAsFactors = FALSE)
              pens <- paste(x[16:length(x)], collapse = "\n")
              df_pens <- read.table(text = pens, stringsAsFactors = FALSE)
              names(df_pens) <- c("display", "interval", "mode", "color" ,"inLegend", "setupCode", "updateCode")
              df_pens <- transform(df_pens,
                name = x[5],
                xaxis = x[6],
                yaxis = x[7],
                stringsAsFactors = FALSE
              )[, c((ncol(df_pens)+1):(ncol(df_pens)+3), 1:(ncol(df_pens)))]
              df_pens
            })
    )

  ret <- list(
    view = view_values,
    sliders = slider_values,
    switches = switches_values,
    monitors = monitors_values,
    plots = plots_values,
    description = model_descriprion,
    netlogo_version = netlogo_version,
    behavior_space = behavior_space
  )
  class(ret) <- c(nl_model_class, class(ret))
  ret
}

