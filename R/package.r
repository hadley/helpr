#' @include memoise.r


# Function to get information about a package.  List of :
#   * parsed description
#   * help topics
#   * data sets
#   * vignettes

# data_sets <- data(package = pkg)$results[, 3]
# vignettes <- vignette(package=pkg)$results[, 3]
# paths <- sapply(vignettes, function(v) vignette(v, package = pkg)$file)


#' Documentation Database Path
#'
#' @return \code{file.path} to the documentation database
#' @param package package to explore
pkg_rddb_path <- function(package) {
  file.path(pkg_help_path(package), package)
}

#' Package Help Path
#'
#' @return \code{file.path} to the help folder
#' @param package package to explore
pkg_help_path <- function(package) {
  system.file("help", package = package)
}


#' Package Vignettes
#'
#' @return \code{\link{subset}} of the \code{\link{vignette()}$results} \code{\link{data.frame}} ("Package", "LibPath", "Item" and "Title")
#' @param package package to explore
pkg_vigs <- function(package) {
  vignettes <- vignette(package = package)$results
  
  if(!NROW(vignettes))
    return(NULL)

  titles <- str_replace(vignettes[,4], "source, pdf", "")
  titles <- str_trim(str_replace(titles, "[()]", ""))
  
  data.frame(item = vignettes[,"Item"], title = titles, stringsAsFactors = FALSE)
}

#' Package Topics Alias to File Index
#'
#' @return \code{\link{data.frame}} containing \code{alias} (function name) and \code{file} that it is associated with
#' @param package package to explore
pkg_topics_index <- memoise(function(package) {
  help_path <- pkg_help_path(package)
  
  file_path <- file.path(help_path, "AnIndex")
  ### check to see if there is anything that exists, aka sinartra
  if(length(readLines(file_path, n = 1)) < 1)
    return(NULL)

  topics <- read.table(file_path, sep = "\t", 
    stringsAsFactors = FALSE, comment.char = "", quote = "", header = TRUE)
  names(topics) <- c("alias", "file") 
  topics[complete.cases(topics), ]
})


#' Package Topics File Documentation
#'
#' Items can be accessed by \code{\emph{list()}$file_name}
#' @return \code{\link{list}} containing the documentation file of each file of a package
#' @param package package to explore
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


#' All Dataset Names
#' 
#' @return \code{\link{vector}} of names that can be a dataset
#' @param package package to explore
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

pkg_internal_function_files <- function(package){
  rd_docs <- pkg_topics_rd(package)
  
  subset(
    names(rd_docs), 
    sapply(rd_docs, function(x){
      identical(as.character(x$keyword[[1]]), "internal")
    })
  )
  
}


#' All Dataset Names
#' 
#' @return \code{\link{list}} of the items in \code{\link{pkg_topics_index}} as \code{function}, \code{dataset}, \code{internal}, \code{package}, or \code{idk}.
#' @param package package to explore
pkg_topics_index_and_type <- function(package){
  types <- pkg_topic_index_type(package)
  
  list(
    package = subset(types, type == "package", select = 1:2),
    help_name = subset(types, type == "help_name", select = 1:2),
    dataset = subset(types, type == "dataset", select = 1:2),
    func = subset(types, type == "function", select = 1:2),
    internal = subset(types, type == "internal", select = 1:2),
    idk = subset(types, type == "IDK", select = 1:2)
  )
}


#' Determines if object is a function
#'
#' @param items items supplied from pkg_topics
#' @return boolean
is_function <- function(package, items){
  sapply(items, function(x){
    exists(x, envir = asNamespace(package), mode="function") |
    exists(x, str_join("package:", package))
  })
}

#' Determines object type
#'
#' Requires the package to be loaded to accurately determine the type of the object
#' @param package package to explore
#' @return returns a type ("package", "dataset", "function", "internal", "help_name" or "IDK")
pkg_topic_index_type <- function(package){
  suppressMessages(require(package, character.only = TRUE))
  
  index <- pkg_topics_index(package)
  
  index$type <- "IDK"
  
  # package
  index[str_detect(index$file, "-package"), "type"] <- "package"

  # dataset
  rows <- with(index,
    file %in% get_datasets(package) |
    alias %in% get_datasets(package)
  )
  
  index[rows,"type"] <- "dataset"
  
  # function
  rows <- with(index, type == "IDK")
  
  func_exists <- as.character(is_function(package, index[rows, "alias"]))
    # Sometimes there are only functions, so factoring doesn't work
  func_exists[func_exists == "TRUE"] <- "function"
  func_exists[func_exists == "FALSE"] <- "IDK"
  index[rows, "type"] <- func_exists
  
  # internal
  rows <- with(index, file %in% pkg_internal_function_files(package))
  index[rows, "type"] <- "internal"
  
  rows <- with(index, type == "IDK")
  help_name <- as.character(sapply(index[rows, "alias"], exists))
    # Sometimes there are only functions, so factoring doesn't work
  help_name[help_name == "TRUE"] <- "IDK"
  help_name[help_name == "FALSE"] <- "help_name"
  index[rows, "type"] <- help_name
  
  index  
}

