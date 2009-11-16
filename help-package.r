# Function to get information about a package.  List of :
#   * parsed description
#   * help topics
#   * data sets
#   * vignettes

# data_sets <- data(package = pkg)$results[, 3]
# vignettes <- vignette(package=pkg)$results[, 3]
# paths <- sapply(vignettes, function(v) vignette(v, package = pkg)$file)


pkg_rddb_path <- function(package) {
  file.path(pkg_help_path(package), package)
}

pkg_help_path <- function(package) {
  system.file("help", package = package)
}

pkg_topics <- function(package) {
  files <- unique(pkg_topics_index(package)$alias)
}

pkg_topics_index <- function(package) {
  help_path <- pkg_help_path(package)

  topics <- read.table(file.path(help_path, "AnIndex"), sep = "\t", 
    stringsAsFactors = FALSE)
  names(topics) <- c("alias", "file")  
  topics
}
