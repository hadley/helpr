#' @include memoise.r

pkg_list <- function() {
  libraryResults <- as.data.frame(library()$results, stringsAsFactors = FALSE)

  # subset to packages that are loaded
  packages <- libraryResults[libraryResults$Package %in% (.packages()), ]
  packages <- packages[order(packages$Package), ]
  lapply(1:nrow(packages), function(i) as.list(packages[i, ]))
}
