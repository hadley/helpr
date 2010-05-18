#' @include memoise.r

pkg_list <- function(all = TRUE) {
  libraryResults <- as.data.frame(library()$results, stringsAsFactors = FALSE)

  # subset to packages that are loaded
  
#  if(!all)
#    packages <- libraryResults[libraryResults$Package %in% loaded_packs(), ]
#  else
    packages <- libraryResults

  packages$isLoaded <- packages$Package %in% loaded_packs()
    
  packages <- packages[order(packages$Package), ]
  lapply(1:nrow(packages), function(i) as.list(packages[i, ]))
}


