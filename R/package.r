#' @include memoise.r


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


pkg_vigs <- function(package) {
  vignettes <- vignette(package = package)$results
  
  if(!has_length(vignettes))
    return(NULL)

  titles <- str_replace(vignettes[,4], "source, pdf", "")
  titles <- str_trim(str_replace(titles, "[()]", ""))
  
  data.frame(item = vignettes[,"Item"], title = titles, stringsAsFactors = FALSE)
}

pkg_topics_index <- memoise(function(package) {
  help_path <- pkg_help_path(package)
  
  file_path <- file.path(help_path, "AnIndex")
  ### check to see if there is anything that exists, aka sinartra
  if(!has_length(readLines(file_path, n = 1)))
    return(NULL)

  topics <- read.table(file_path, sep = "\t", 
    stringsAsFactors = FALSE, comment.char = "", quote = "", header = TRUE)
  names(topics) <- c("alias", "file") 
  topics[complete.cases(topics), ]
})

pkg_topics_rd <- memoise(function(package) {
  rd <- tools:::fetchRdDB(pkg_rddb_path(package))
  lapply(rd, name_rd)
})

# 
# index <- compact(llply(plyr, function(rd) {
#   keywords <- reconstruct(untag(rd$keyword))
#   if (any(keywords != "internal")) return()
#   
#   list(
#     name = reconstruct(untag(rd$name)),
#     title = reconstruct(untag(rd$title)),
#     aliases = unname(sapply(rd[names(rd) == "alias"], "[[", 1)),
#     keywords = unname(sapply(rd[names(rd) == "keyword"], "[[", 1))
#   )
# }))
# str(index)


#' Get All Dataset Names
#' 
#' @param package package to explore
#' @return collection of names that can be a dataset
get_datasets <- function(package){
  sets <- suppressWarnings(data(package = package)$results)
  
  # make sure it gets the right package (base and datasets)
  items <- sets[sets[,"Package"] == package, "Item"]
  
  # extract names of datasets that belong to a group, 
  #   i.e. beaver1 (beaver) becomes c("beaver1", "beaver")
  data_set_names <- function(name){
    str_extract_all(name, "[a-zA-Z_.][a-zA-Z_.0-9]*")[[1]]
  }

  data_sets <- as.character(unique(unlist(sapply(items, data_set_names))))
  data_sets[order(data_sets)]
}


pkg_topics_index_and_type <- function(package){
  types <- pkg_topic_index_type(package)
  
  list(
    func = types[types$type == "function", 1:2],
    dataset = types[types$type == "dataset", 1:2],
    internal = types[types$type == "internal", 1:2],
    idk = types[types$type == "IDK", 1:2],
    package = types[types$type == "package", 1:2]
  )
}


#' Determines if object is a function
#'
#' @param items items supplied from pkg_topics
#' @return boolean
is_function <- function(items){
  exists(items, mode="function")
}

#' Determines object type
#'
#' @param package package to explore
#' @return returns a type ("package", "
pkg_topic_index_type <- function(package){
  index <- pkg_topics_index(package)
  
  index$type <- "IDK"
  
  # dataset
  rows <- with(index,
    file %in% get_datasets(package) |
    alias %in% get_datasets(package)
  )
  
  index[rows,"type"] <- "dataset"
  
  # function
  rows <- index$type == "IDK"
  func_exists <- as.character(is_function(index[rows, "alias"]))
  func_exists[func_exists == "TRUE"] <- "function"
  func_exists[func_exists == "FALSE"] <- "IDK"
#  factor(func_exists, labels = c("IDK", "function"))
  index[rows, "type"] <- func_exists
  
  # internal
  rows <- str_sub(index$alias, start = 0, end = 1) == "."
  index[rows, "type"] <- "internal"
  
  # package
  index[str_detect(index$file, "-package"), "type"] <- "package"

  index    
  
}

