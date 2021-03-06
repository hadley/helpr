#' Installed packages.
#'
#' Runs about 10x faster than \code{\link{installed.packages}}
#' @return a list of packages
#' @keywords internal
#' @author Hadley Wickham
installed_packages <- function() {
  user_libPaths <- normalizePath(.libPaths())
  uniqueLibPaths <- subset(user_libPaths, !duplicated(user_libPaths))
  
  paths <- unlist(lapply(uniqueLibPaths, dir, full.names = TRUE))
  desc <- file.path(paths, "DESCRIPTION")
  desc <- desc[file.exists(desc)]
  
  dcf <- lapply(desc, read.dcf, fields = c("Package", "Title", "Version"))
  packages <- as.data.frame(do.call("rbind", dcf), stringsAsFactors = FALSE)
  
  packages$status <- ifelse(packages$Package %in% .packages(), "loaded", "installed")
  class(packages) <- c("packages", class(packages))
  packages <- packages[order(packages$Package), ]
  packages[! duplicated(packages$Package), ]
  
}


#' Out of date packages.
#'
#' @author Barret Schloerke
#' @keywords internal
old_package_names <- function() {
  unname(old.packages()[, "Package"])
}

#' Update all packages that are old and currently loaded or installed.
#'
#' @author Barret Schloerke
#' @keywords internal
update_packs <- function(all = FALSE) {
  if(all) {
    packs <- installed_packages()[,"Package"]
  } else {
    packs <- .packages()
  }
  packs <- packs[packs %in% old_package_names()]
  install_packages(packs)
  as.array(packs)
}

#' Install packages.
#' update a vector of packages
#'
#' @author Barret Schloerke
#' @keywords internal
install_packages <- function(pkg_vec) {
#  message(str_c("installing: ", pkg_vec, collapse = "\n"))
  suppressWarnings(install.packages(pkg_vec, repos = "http://cran.r-project.org/"))
}
