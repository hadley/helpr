#' @include memoise.r

pkg_list <- memoise(function() {
  packages <- as.data.frame(library()$results, stringsAsFactors = FALSE)
  packages <- packages[order(packages$Package), ]
  lapply(1:nrow(packages), function(i) as.list(packages[i, ]))
})