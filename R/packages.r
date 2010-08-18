#' Installed packages.
#'
#' Runs about 10x faster than \code{\link{installed.packages}}
#' @return a list of packages
#' @keywords internal
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


#' Out of date packages.
#' Local packages that need updating.
#'
#' @keywords internal
old_package_names <- function() {
  as.list(old.packages()[,"Package"])
}

#' Update old packages
#' update all packages that are old and currently loaded or installed
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
update_packs <- function(all = FALSE) {
  if(all) {
    packs <- installed_packages()[,"Package"]
  } else {
    packs <- .packages()
  }
  packs <- packs[packs %in% old_package_names()]
  install_packages(packs)
  as.list(packs)
}

#' install packages
#' update a vector of packages
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
install_packages <- function(pkg_vec){
  suppressWarnings(install.packages(pkg_vec, repos = "http://cran.r-project.org/"))
}
