



build_topic <- function(rd_file, site_path, header, footer) {

  out_file <- file.path(site_path,
                        gsub("\\.Rd$", ".html", basename(rd_file)))
  tools::Rd2HTML(rd_file, out = out_file )

  library(XML)
  doc <- XML::xmlParse(out_file)
  body <- XML::getNodeSet(
    doc, "//x:body", namespaces = c(x = "http://www.w3.org/1999/xhtml"))[[1]]
  XML::saveXML(body, out_file )
  cat("\n", file = out_file, append = TRUE)
  body_internal <- readLines(out_file)[-1]
  cat( header, file = out_file, sep = "\n")
  cat( body_internal, file = out_file, sep = "\n", append = TRUE)
  cat( footer, file = out_file, sep = "\n", append = TRUE)
}

get_header <- function(package_path, sample_file = "ants.html") {
  a1 <- readLines(file.path(package_path,"pages", sample_file))
  last_line <- grep("<!--/\\.navbar -->", a1)
  header <- a1[1:(last_line+1)]

  #change relative links to ../
  header <-
    ifelse(grepl("http", header),
           header,
           gsub("href=\"", "href=\"../", header ))
  header <-
    ifelse(grepl("http", header),
           header,
           gsub("<script src=\"libs/", "<script src=\"../libs/", header ))

  header <-
    ifelse(grepl("<title>", header),
           gsub("<title>Ants</title>", "<title>Help</title>", header ),
           header)

  header
}

get_footer <- function(package_path, sample_file = "ants.html") {
  a1 <- readLines(file.path(package_path,"pages", sample_file))
  last_line <- grep("<footer>", a1)
  header <- a1[last_line:(length(a1))]

  #change relative links to ../
  header <-
    ifelse(grepl("http", header),
           header,
           gsub("href=\"", "href=\"../", header ))

  header
}


build_help_site <- function(package_path, site_path) {

  topic_files <- list.files(file.path(package_path, "man"), pattern = "\\.Rd$")
  topic_files <- file.path(package_path, "man", topic_files)

  header <- get_header(package_path)
  footer <- get_footer(package_path)

  for(f in topic_files) {
    build_topic(f, site_path, header, footer)
  }

}




#tools::Rd2HTML(rd_file, out = out_file )

get_title <- function(rd_content, key = "\\title") {
  unname(
    as.character(
      rd_content[
        sapply(rd_content, function(x) attr(x, "Rd_tag") == key) ][[1]][[1]])
  )
}

get_value <- function(rd_content, key) {
  find_key <- sapply(rd_content, function(x) attr(x, "Rd_tag") == "\\keyword" )
  if(!any(find_key))
    return("")

  value <- rd_content[find_key][[1]]
  if(length(value) > 0)
    return(value[[1]])
  return("")
}


build_help_index <- function(package_path, site_path) {

  library(staticdocs)
  pkg1 <- as.sd_package(package_path, site_path = site_path)
  ind <- pkg1$rd_index
  ind$title <- sapply(pkg1$rd, get_title)
  ind$keyword <- sapply(pkg1$rd, get_value, "\\keyword")
  out_file <- file.path(site_path, "index.html")

  header <- get_header(package_path)
  footer <- get_footer(package_path)

  ind <- do.call(rbind,
    lapply(seq_along(ind$name), function(i) {
      data.frame(name = ind$name[i],
                 title = ind$title[i],
                 alias = ind$alias[[i]],
                 file_out = ind$file_out[i],
                 keyword = ind$keyword[i])
    })
  )
  ind$link <- paste0(ind$name, ".html")
  ind <- ind[ind$keyword == "", ]
  sort1 <-
    grepl("nlexperiment", ind$name)*(-1) +
    grepl("nl_get", ind$name)*1 +
    grepl("nl_show", ind$name)*1.1 +
    grepl("nl_set", ind$name)*2 +
    grepl("nl_eval", ind$name)*3 +
    grepl("nl_import", ind$name) * 4 +
    grepl("nl_export", ind$name) * 5
  ind <- ind[order(sort1), ][-1, ]
  cat(header, sep = "\n", file = out_file)
  cat(
    sprintf("<p> <div><a href=\"%s\">%s</a></div> %s </p>",ind$file_out, ind$alias, ind$title),
          sep = "\n", file = out_file, append = TRUE)

  cat(footer, sep = "\n", file = out_file, append = TRUE)
}



