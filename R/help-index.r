#' @include memoise.r

pkg_list <- function() {
  libraryResults <- as.data.frame(library()$results, stringsAsFactors = FALSE)
  packages <- libraryResults[libraryResults$Package %in% (.packages()), ]
  packages <- packages[order(packages$Package), ]
  packages <- lapply(1:nrow(packages), function(i) as.list(packages[i, ]))
  bprint(packages)
  packages
}

old_pkg_list <- function() {
  old_packages <- as.data.frame(old.packages(), stringsAsFactors = FALSE)
  bprint(old_packages)
  old_packages
}