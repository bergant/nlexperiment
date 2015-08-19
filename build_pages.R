build_pages <- function(pages_path) {

  library(rmarkdown)
  render_page <- function(p) {
    rmarkdown::render(p)
  }

  pages <- list.files(path = pages_path, pattern = "\\.rmd$" )
  pages <- file.path(pages_path, pages)
  for(p in pages) {
    render_page(p)
  }
}

pages_path <- "~/R/GitHub/nlexperiment/pages"
build_pages(pages_path)
