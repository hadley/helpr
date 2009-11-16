pkg_rddb_path <- function(package) {
  file.path(pkg_help_path(package), package)
}

pkg_help_path <- function(package) {
  system.file("help", package = package)
}

pkg_topic <- function(package, topic, file = NULL) {
  if (is.null(file)) {
    topics <- pkg_topics_index(package)
    file <- topics$file[topics$alias == topic]
    stopifnot(length(file) == 1)    
  }
  
  tools:::fetchRdDB(pkg_rddb_path(package), file)
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


pkgs <- function() {
  
}