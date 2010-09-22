#' Helpr Package
#'
#' @aliases helpr_package helpr_package_mem
#' @return all the information necessary to produce a package site ("/package/:package/")
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
helpr_package <- function(package) {
  helpr_package_mem(package, pkg_version(package))
}

helpr_package_mem <- memoise(function(package, version) {
  
  info <- .readRDS(system.file("Meta", "package.rds", package = package))
  description <- as.list(info$DESCRIPTION)
  info$DESCRIPTION <- NULL
  description <- modifyList(info, description)
  names(description) <- tolower(names(description))
  
  author_str <- pluralize("Author", description$author)
  
  items <- pkg_topics_alias(package)
  
  demos <- pkg_demos(package)
  vigs <- pkg_vigs(package)

  description$depends <- parse_pkg_desc_item(description$depends)
  description$imports <- parse_pkg_desc_item(description$imports)
  description$suggests <- parse_pkg_desc_item(description$suggests)
  description$extends <- parse_pkg_desc_item(description$extends)
  description$reverse <- tools:::dependsOnPkgs(package)
  description$author <- pkg_author_and_maintainers(description$author, description$maintainer)
  description$maintainer <- NULL

  if (has_text(description$url)) {
    description$url <- str_trim(str_split(description$url, ",")[[1]])
  }

  list(
    package = package, 
    items = items,
    description = description, 
    author_str = author_str, 
    demos_str = pluralize("Demo", demos),
    demos = demos,
    vigs_str = pluralize("Vignette", vigs),
    vigs = vigs,
    change_log = pkg_news(package)
  )
})

#' Package Version
#' returns the package version from the rd file
#'
#' @keywords internal
#' @author Hadley Wickham
pkg_version <- function(pkg) {
  .readRDS(system.file("Meta", "package.rds", package = pkg))$DESCRIPTION[["Version"]]
}

#' Parse a package description
#' makes sure that a package version is properly displayed if it is not given in a nice format
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
parse_pkg_desc_item <- function(obj) {
  if (NROW(obj) < 1) {
    return(NULL)
  }
  
  if (!is.list(obj)) {
    obj <- list(obj = (list(name = obj, version = NULL)))
  }

  as.data.frame(
    sapply(obj, function(x) {
      vers <- NULL
      
      # if the version is found, it will create one in the form of '(1.2.3)'
      if (!is.null(x$version)) {
        vers <- str_c("(", x$op, " ", str_c(unclass(x$version)[[1]], collapse = "."), ")", collapse = "")
      }
      list(name = as.character(x$name), version = as.character(vers))
    })
    , stringsAsFactors = FALSE
  )
}

#' Parse Authors and Maintainer
#'
#' @param description list containing the \code{author} and \code{maintainer}
#' @return string containing the authors with links properly displayed
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
pkg_author_and_maintainers <- function(authors, maintainer=NULL) {

  # retrieve the authors and email
  authors <- str_replace_all(authors, "\\n", " ")
  str_extract_all(authors, "[a-zA-z]* [a-zA-z]* <*>")

  # retrieve the author and email separately
  all_pattern <- "[a-zA-Z][a-zA-Z]* [a-zA-Z][a-zA-Z]* <[a-zA-Z0-9._-]*@[a-zA-Z0-9._-]*.[a-zA-Z]{2,7}>"
  name_pattern <- "[a-zA-Z][a-zA-Z_-]*[ ]{0,1}[a-zA-Z][a-zA-Z]*"
  email_pattern <- "<[a-zA-Z0-9._-]*@[a-zA-Z0-9._-]*.[a-zA-Z]{2,7}>"
  auths_string <- str_extract_all(authors, all_pattern)[[1]]
  auths <- sapply(str_extract_all(auths_string, name_pattern), "[[", 1)
  emails <- sapply(str_extract_all(auths_string, email_pattern), "[[", 1)
  if (length(emails) < 1) {
    author_and_email <- auths
  } else {
    emails <- str_replace_all(emails, "<", "")
    emails <- str_replace_all(emails, ">", "")
    
    author_and_email <- author_email(auths, emails)
  }
  
  # replace the original authors with the linked authors
  for (i in seq_along(author_and_email)) {
    authors <- str_replace_all(authors, auths_string[i], author_and_email[i])
  }

  if (!is.null(maintainer)) {
    maintainer_name <- str_trim(strip_html(maintainer))
    maintainer_email <- str_extract_all(maintainer, email_pattern)[[1]][1]
    maintainer_email <- str_replace_all(maintainer_email, "<", "")
    maintainer_email <- str_replace_all(maintainer_email, ">", "")
  
    # make the maintainer bold
    maintainer_string <- str_c("<strong>", author_email(maintainer_name, maintainer_email), "</strong>", collapse = "")  
  
    if (str_detect(authors, maintainer_name)) {
      # replace the current author text with the maintainer text
      authors <- str_replace_all(
        authors, 
        str_c("</?.*?>",maintainer_name,"</?.*?>", collapse = ""),
        maintainer_name
      )
      
      authors <- str_replace_all(
        authors, 
        maintainer_name,
        maintainer_string
      )
    } else {
      # attach the maintainer to the end
      authors <- str_c(authors, "; ", maintainer_string, collapse = "")
    }
  }
  authors
}

#' Documentation Database Path
#'
#' @param package package to explore
#' @return \code{file.path} to the documentation database
#' @keywords internal
#' @author Hadley Wickham
pkg_rddb_path <- function(package) {
  file.path(pkg_help_path(package), package)
}

#' Package Help Path
#'
#' @param package package to explore
#' @return \code{file.path} to the help folder
#' @keywords internal
#' @author Hadley Wickham
pkg_help_path <- function(package) {
  system.file("help", package = package)
}


#' Package Vignettes
#'
#' @param package package to explore
#' @return \code{subset} of the \code{vignette()$results} \code{data.frame} ("Package", "LibPath", "Item" and "Title")
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
pkg_vigs <- function(package) {
  vignettes <- vignette(package = package)$results
  
  if (!NROW(vignettes)) {
    return(NULL)
  }

  titles <- str_replace_all(vignettes[,4], "source, pdf", "")
  titles <- str_trim(str_replace_all(titles, "[()]", ""))
  
  data.frame(item = vignettes[,"Item"], title = titles, stringsAsFactors = FALSE)
}

#' Package Topics Alias to File Index
#'
#' @param package package to explore
#' @return \code{\link{data.frame}} containing \code{alias} (function name) and \code{file} that it is associated with
#' @keywords internal
#' @author Hadley Wickham
pkg_topics_index <- memoise(function(package) {
  help_path <- pkg_help_path(package)
  
  file_path <- file.path(help_path, "AnIndex")
  ### check to see if there is anything that exists, aka sinartra
  if (length(readLines(file_path, n = 1)) < 1) {
    return(NULL)
  }

  topics <- read.table(file_path, sep = "\t", 
    stringsAsFactors = FALSE, comment.char = "", quote = "", header = FALSE)
    
  names(topics) <- c("alias", "file") 
  topics[complete.cases(topics), ]
})


#' Package Topics File Documentation
#'
#' Items can be accessed by \emph{\code{list()}}\code{$file_name}
#' @param package package to explore
#' @return \code{\link{list}} containing the documentation file of each file of a package
#' @keywords internal
#' @author Hadley Wickham
pkg_topics_rd <- memoise(function(package) {
  rd <- tools:::fetchRdDB(pkg_rddb_path(package))
  lapply(rd, name_rd)
})

#' Topic Title and Aliases by Package
#' return information on the package, datasets, internal, and datasets
#'
#' @param pkg package in question
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
pkg_topics_alias <- function(pkg) {
    
  rd1 <- pkg_topics_rd(pkg)
  rd <- lapply(rd1, function(x) {
    desc <- reconstruct(untag(x$description))
    desc_naked <- strip_html(desc)
    if(str_length(desc_naked) > 100) {
      desc <- str_c(str_sub(desc_naked, end = 100), " ...")
    }
    list(
      alias = unname(sapply(x[names(x) == "alias"], "[[", 1)),
      keywords = str_trim(reconstruct(untag(x$keyword))),
      desc = desc,
      title = reconstruct(untag(x$title))
    )
  })
  keywords <- sapply(rd, function(x){x$keywords })
          
  package_info <- rd[keywords == "package"]
  internal <- rd[keywords == "internal"]
  dataset <- rd[keywords == "datasets"]  
  
  rows <- keywords %in% c("package", "internal", "datasets")
  if(sum(rows) > 0)
    rd[rows] <- NULL 
  
  list(func = rd, dataset = dataset, internal = internal, info = package_info)
}



#' All Dataset Names
#' 
#' @return \code{\link{vector}} of names that can be a dataset
#' @param package package to explore
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
get_datasets <- function(package){
  sets <- suppressWarnings(data(package = package)$results)
  
  # make sure it gets the right package (base and datasets)
  items <- sets[sets[,"Package"] == package, "Item"]
  
  if(length(items) < 1) return(list())
  
  # extract names of datasets that belong to a group, 
  #   i.e. beaver1 (beaver) becomes c(dataset = "beaver1", group = "beaver")
  split_names <- str_extract_all(items, "[a-zA-Z_.][a-zA-Z_.0-9]*")
  split_names_labeled <- lapply(split_names, function(x){
    if(length(x) > 1) {
      c(dataset = x[1], help_name = x[2])
    } else {
      c(dataset = x[1])      
    }
  })
  
  split_names_labeled <- unlist(split_names_labeled)
  data_and_group <- split(split_names_labeled, names(split_names_labeled))
  
  if(!is.null(data_and_group$dataset)) {
    data_and_group$dataset <- data_and_group$dataset[order(data_and_group$dataset)]
  }
  if(!is.null(data_and_group$help_name)) {
    data_and_group$help_name <- data_and_group$help_name[order(data_and_group$help_name)]
  }
  
  data_and_group
}
 
#' package internal functions
#' retrieve all package internal functions
#'
#' @param package package in question
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
pkg_internal_function_files <- function(package){
  rd_docs <- pkg_topics_rd(package)
  
  subset(
    names(rd_docs), 
    sapply(rd_docs, function(x){
      identical(reconstruct(untag(x$keyword)), "internal")
    })
  )
  
}


#' All Dataset Names
#' 
#' @return \code{\link{list}} of the items in \code{\link{pkg_topics_index}} as \code{function}, \code{dataset}, \code{internal}, \code{package}, or \code{NA}.
#' @param package package to explore
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
pkg_topics_index_by_type <- function(package){
  types <- pkg_topic_index_type(package)
  split(types[,1:2], types[,"type"])
}


#' Find all top level package usage functions
#'
#' @param usage usage in question
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
package_usage_functions <- function(usage){
  
  p_text <- attr(parser(text = reconstruct(untag(usage))), "data")
  text_top_levels <- split(p_text, p_text[,"top_level"])

  unname(sapply(text_top_levels, function(x){
    rows <- with(x, token.desc == "SYMBOL_FUNCTION_CALL")
    item_row <- x[rows, ]
    if(nrow(item_row) < 1) {
      NULL
    } else {
      item_row[1,"text"]
    }
      
  }))
}


#' Determines if object is a function
#'
#' @param items items supplied from pkg_topics
#' @return boolean
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
is_function <- function(package, items){
#  
#  # retrieve the files of all items in question
#  index <- pkg_topics_index(package)
#  index <- subset(index, alias %in% items)
#  files <- unique(index[,"file"])
#  
#  rd_files <- pkg_topics_rd(package)
#
#  pattern <- "[a-zA-Z_.][a-zA-Z_.0-9]*[ ]*\\(" 
#  
#  # for every file... 
#  functions <- lapply(files, function(rd){
#    rd_file <- rd_files[[rd]]
#    
#    # get all the functions and methods in the usage
#    funcs <- retrieve_usage_functions_and_methods(rd_file[["usage"]])
#    
#    # get all the aliases of the file
#    aliases <- unname(sapply(rd_file[names(rd_file) == "alias"], "[[", 1))
#    aliases <- c(rd, aliases)
##    print(rd)
##    print(aliases)
##    print(funcs)
#
#    # select the functions and aliases that have characters
#    funcs <- str_replace_all(funcs, "\\[", "\\\\[")
#    funcs <- funcs[!str_detect(funcs, " ")]
#
#    funcs <- funcs[str_detect(funcs, "[a-zA-Z]")]
#    funcs <- funcs[!str_detect(funcs, "\\[")]
#    funcs <- funcs[!str_detect(funcs, "\\$")]
#    funcs <- funcs[!is.na(funcs)]
#    
#    aliases <- aliases[str_detect(aliases, "[a-zA-Z]")]
#    aliases <- aliases[!str_detect(aliases, "\\[")]
#    aliases <- aliases[!str_detect(aliases, "\\$")]
#    aliases <- aliases[!is.na(aliases)]
##    print(aliases)
##    print(funcs)
#    if(length(aliases) < 1)
#      return(NULL)
#    
#    
#    # see which aliases are in the functions
#    contains_function <- sapply(funcs, function(x) str_detect(aliases, x))
#    if(length(contains_function) < 1) {
#      exists <- rep(FALSE, length(aliases))
#    } else if(is.null(ncol(contains_function))) {
#      exists <- contains_function
#    } else {
#      exists <- apply(contains_function, 1, any)
#    }    
#        
#    names(exists) <- aliases
##    print(exists)
#
#    # return the name of the alias and whether or not it is a function
#    exists
#
#  })
#
#  # pick only the 'FALSE' or non function items and get their names
#  functions_v <- unlist(functions)
#  names_of_groups <- names(functions_v[functions_v == FALSE])
#  
#  # return the type of the aliases, in the same order they were given
#  type <- rep("function", length(items))
#  for(i in names_of_groups) {
#    type[items == i] <- "help_name"
#  }
#  type
#  
#  # since the above method doesn't work, the old one is still in place
  
# OLD METHOD  
  library(package, character.only = TRUE)
  sapply(items, function(x){
    if(exists(x, str_c("package:", package)))
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
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
pkg_topic_index_type <- function(package){
  
  index <- pkg_topics_index(package)
  
  index$type <- "NA"
  
  # package
  index[str_detect(index$file, "-package"), "type"] <- "package"
  rows <- with(index, type == "package")
  if(sum(rows) > 1){
    index <- subset(index, !str_detect(index$alias, "-package"))
  }

  # dataset
  pkg_datas <- get_datasets(package)  
  if(!is.null(pkg_datas$dataset)) {
    rows <- with(index, alias %in% pkg_datas$dataset )
    index[rows,"type"] <- "dataset"    
  }
  if(!is.null(pkg_datas$help_name)) {
    rows <- with(index, alias %in% pkg_datas$help_name )
    index[rows,"type"] <- "help_name"    
  }
  
  
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





#' Find all usage functions and methods
#'
#' @param usage usage in question
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
retrieve_usage_functions_and_methods <- function(usage){
  if(reconstruct(untag(usage)) == "")
    return(NULL)
    
  c(usage_functions(usage), str_c(".", usage_methods(usage)) )
}


#' function levels
#' go through the function text to find the function level (depth) of each function
#'
#' @param text text to be evaluated
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
function_levels <- function(text){
  split_text <- str_split(text, "")[[1]]
  
  value <- 0
  text_value <- integer(length(split_text))
  text_value[1] <- 0

  for(i in 2:length(split_text)) {
    if(split_text[i-1] == "(") {
      value <- value + 1
    } 
    if(split_text[i] == ")") {
      value <- value - 1
    } else {
      value <- value      
    }
    text_value[i] <- value
  }

  text_value  
}

#' Find all usage functions
#'
#' @param usage usage in question
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
usage_functions <- function(usage){
  usage <- reconstruct(untag(usage))
  if(str_trim(usage) == "")
    return(NULL)
  
  split_usage <- str_split(usage, "")[[1]]
  # find the function level of each function
  usage_level <- function_levels(usage)
  
  # split each function by "\n", after it has been trimmed, and only using the top level
  usage_functions <- split_usage[usage_level == 0]
  usage_functions <- str_c(usage_functions, collapse ="")
  usage_functions <- str_trim(usage_functions)
  usage_functions <- str_split(usage_functions, "\n")[[1]]
  
  # remove unwanted characters
  usage_functions <- str_replace_all(usage_functions, "\\(", "")
  usage_functions <- str_replace_all(usage_functions, "\\)", "")
  # remove commented lines
  usage_functions <- usage_functions[ str_sub(usage_functions, end = 1) != "#" ]
  #remove useless functions
  usage_functions <- usage_functions[ usage_functions != "" ]

  usage_functions
}

#' usage methods
#' find all methods within a usage
#' 
#' @param usage usage in question
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
usage_methods <- function(usage) {
  if(str_trim(reconstruct(untag(usage))) == "")
    return(NULL)
  
  methos <- usage[list_tags(usage) == "\\method"]
  methos <- sapply(methos, function(x) { reconstruct(x[[2]]) } )
  unique(methos)
  
}
