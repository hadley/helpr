#' Installed packages.
#'
#' Runs about 10x faster than \code{\link{installed.packages}}
#' @return a list of packages
installed_packages <- function() {
  paths <- unlist(lapply(.libPaths(), dir, full.names = TRUE))
  desc <- file.path(paths, "DESCRIPTION")
  desc <- desc[file.exists(desc)]
  
  dcf <- lapply(desc, read.dcf, fields = c("Package", "Title", "Version"))
  packages <- as.data.frame(do.call("rbind", dcf), stringsAsFactors = FALSE)
  
  packages$status <- ifelse(packages$Package %in% .packages(), 
    "loaded", "installed")
  class(packages) <- c("packages", class(packages))
  packages[order(packages$Package), ]
}

as.list.packages <- function(x) {
  alply(x, 1, as.list)
}

#' Out of date packages.
#' Local packages that need updating.
old_package_names <- function() {
  as.list(as.data.frame(old.packages(), stringsAsFactors = FALSE)$Package)
}

#' Update old packages
#' update all packages that are old and currently loaded or installed
#'
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#'
update_packs <- function(all = FALSE) {
  if(all) {
    packs <- installed_packages()[,"Package"]
  } else {
    packs <- .packages()
  }
  packs <- packs[packs %in% old_package_names()]
  message(str_c("installing: ", packs, collapse = "\n"))
#  install.packages(packs)
  as.list(packs)
}
