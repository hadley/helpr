#' @include memoise.r

pkg_list <- memoise(function() {
  libraryResults <- as.data.frame(library()$results, stringsAsFactors = FALSE)
  packages <- libraryResults[libraryResults$Package %in% (.packages()), ]
  packages <- packages[order(packages$Package), ]
  lapply(1:nrow(packages), function(i) as.list(packages[i, ]))
})