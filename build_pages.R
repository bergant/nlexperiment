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

message("Building pages...")

pages_path <- "~/R/GitHub/nlexperiment/pages"
build_pages(pages_path)

message("Building help...")

source(file.path(pages_path, "build_help.R"))
package_path <- "C:/Users/dare/Documents/R/GitHub/nlexperiment"
site_path <- file.path(package_path, "pages", "help")
build_help_site(package_path, site_path)
build_help_index(package_path, site_path)
