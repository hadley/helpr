#' Helpr Package
#'
#' @return all the information necessary to produce a package site ("/package/:package/")
helpr_package <- function(package){
  
  info <- .readRDS(system.file("Meta", "package.rds", package = package))
  description <- as.list(info$DESCRIPTION)
  info$DESCRIPTION <- NULL
  description <- defaults(info, description)
  names(description) <- tolower(names(description))
  
  author_str <- pluralize("Author", description$author)
  
  items <- pkg_topics_index_by_type(package)
  
  demos <- pkg_demos(package)
  vigs <- pkg_vigs(package)

  description$depends <- parse_pkg_desc_item(description$depends)
  description$imports <- parse_pkg_desc_item(description$imports)
  description$suggests <- parse_pkg_desc_item(description$suggests)
  description$extends <- parse_pkg_desc_item(description$extends)
  description$reverse <- dependsOnPkgs(package)
  description$author <- pkg_author_and_maintainers(description)
#  description$maintainer <- NULL
  

  list(
    package = package, 
    items = items,
    description = description, 
    author_str = author_str, 
    demos_str = pluralize("Demo", demos),
    demos = demos,
    vigs_str = pluralize("Vignette", vigs),
    vigs = vigs
  )
}

parse_pkg_desc_item <- function(obj){
  if(NROW(obj) < 1){
    return(NULL)
  }
  
  if(!is.list(obj)){
    obj <- list(obj = (list(name = obj, version = NULL)))
  }

  as.data.frame(
    sapply(obj, function(x){
      vers <- NULL
      if(!is.null(x$version))
        vers <- str_join("(", x$op, " ", str_join(unclass(x$version)[[1]], collapse = "."), ")", collapse = "")
      list(name = as.character(x$name), version = as.character(vers))
    })
    , stringsAsFactors = FALSE
  )
}

#' Parse Authors and Maintainer
#'
#' @param description list containing the \code{author} and \code{maintainer}
#' @return string containing the authors with links properly displayed
pkg_author_and_maintainers <- function(description){

  # retrieve the authors and email
  authors <- description$author
  authors <- str_replace(authors, "\\n", " ")
  str_extract_all(authors, "[a-zA-z]* [a-zA-z]* <*>")

  # retrieve the author and email separately
  all_pattern <- "[a-zA-Z][a-zA-Z]* [a-zA-Z][a-zA-Z]* <[a-zA-Z0-9._-]*@[a-zA-Z0-9._-]*.[a-zA-Z]{2,7}>"
  name_pattern <- "[a-zA-Z][a-zA-Z]* [a-zA-Z][a-zA-Z]*"
  email_pattern <- "<[a-zA-Z0-9._-]*@[a-zA-Z0-9._-]*.[a-zA-Z]{2,7}>"
  auths_string <- str_extract_all(authors, all_pattern)[[1]]
  auths <- sapply(str_extract_all(auths_string, name_pattern), "[[", 1)
  emails <- sapply(str_extract_all(auths_string, email_pattern), "[[", 1)
  if(length(emails) < 1){
    author_and_email <- auths
  }else{
    emails <- str_replace(emails, "<", "")
    emails <- str_replace(emails, ">", "")
    
    author_and_email <- author_email(auths, emails)
  }
  
  # make the maintainer bold
  maintainer <- str_extract_all(description$maintainer, name_pattern)[[1]][1]
  pos <- auths == maintainer
  author_and_email[pos] <- str_join("<strong>", author_and_email[pos], "</strong>", collapse = "")
  
  # replace the original authors with the linked authors
  for(i in seq_along(author_and_email)){
    authors <- str_replace(authors, auths_string[i], author_and_email[i])
  }

  authors
}



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
#' @param package package to explore
#' @return \code{file.path} to the documentation database
#'
pkg_rddb_path <- function(package) {
  file.path(pkg_help_path(package), package)
}

#' Package Help Path
#'
#' @param package package to explore
#' @return \code{file.path} to the help folder
#'
pkg_help_path <- function(package) {
  system.file("help", package = package)
}


#' Package Vignettes
#'
#' @param package package to explore
#' @return \code{\link{subset}} of the \code{\link{vignette()}$results} \code{\link{data.frame}} ("Package", "LibPath", "Item" and "Title")
#' 
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
#' @param package package to explore
#' @return \code{\link{data.frame}} containing \code{alias} (function name) and \code{file} that it is associated with
#'
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
#' @param package package to explore
#' @return \code{\link{list}} containing the documentation file of each file of a package
#' 
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
#' @return \code{\link{list}} of the items in \code{\link{pkg_topics_index}} as \code{function}, \code{dataset}, \code{internal}, \code{package}, or \code{NA}.
#' @param package package to explore
pkg_topics_index_by_type <- function(package){
  types <- pkg_topic_index_type(package)
  split(types[,1:2], types[,"type"])
}


#' Determines if object is a function
#'
#' @param items items supplied from pkg_topics
#' @return boolean
is_function <- function(package, items){
  sapply(items, function(x){
    if(exists(x, str_join("package:", package)))
      "function"
    else if(package %in% loadedNamespaces()){
      if(exists(x, envir = asNamespace(package), mode="function")){
        "function"
      }else{
        "NA"
      }
    }else{
      "NA"
    }
  })
}

#' Determines object type
#'
#' Requires the package to be loaded to accurately determine the type of the object
#' @param package package to explore
#' @return returns a type ("package", "dataset", "function", "internal", "help_name" or "NA")
pkg_topic_index_type <- function(package){
  suppressMessages(require(package, character.only = TRUE))
  
  index <- pkg_topics_index(package)
  
  index$type <- "NA"
  
  # package
  index[str_detect(index$file, "-package"), "type"] <- "package"

  # dataset
  rows <- with(index,
    file %in% get_datasets(package) |
    alias %in% get_datasets(package)
  )
  
  index[rows,"type"] <- "dataset"
  
  # function
  rows <- with(index, type == "NA")
  index[rows, "type"] <- is_function(package, index[rows, "alias"])
  
  # internal
  rows <- with(index, file %in% pkg_internal_function_files(package))
  index[rows, "type"] <- "internal"
  
  rows <- with(index, type == "NA")
  help_name <- as.character(sapply(index[rows, "alias"], exists))
    # Sometimes there are only functions, so factoring doesn't work
  help_name[help_name == "TRUE"] <- "NA"
  help_name[help_name == "FALSE"] <- "help_name"
  index[rows, "type"] <- help_name
  
  index  
}

