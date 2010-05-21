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

pkg_topics <- function(package) {
  files <- unique(pkg_topics_index(package)$alias)
}

pkg_vigs <- function(package) {

  vignettes <- vignette(package= package)$results[, 3]
  paths <- sapply(vignettes, function(v) vignette(v, package = package)$file)

  paths <- sapply(paths, function(i){
    if(str_sub(i, str_length(i)-3, str_length(i)) != ".pdf"){
      str_c(str_sub(i, end = str_length(i)-4), ".pdf")
    }else{
      i
    }
  })
  
  paths
    
}



pkg_topics_index <- function(package) {
  help_path <- pkg_help_path(package)

  topics <- read.table(file.path(help_path, "AnIndex"), sep = "\t", 
    stringsAsFactors = FALSE, comment.char = "", quote = "", header = TRUE)
  names(topics) <- c("alias", "file") 
  topics[complete.cases(topics), ]
}

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

has_length <- function(x){
  length(x) > 0 && x != ""
}




get_datasets <- function(package){
#  topics[is_function(topics) == 0]
  sets <- data(package = package)$results
  
  # make sure it gets the right package (base and datasets)
  sets[sets[,"Package"] == package, "Item"]
}

get_functions <- function(topics){
  topics[is_function(topics) == 1]
}

get_internal <- function(topics){
  topics[is_function(topics) == 2]
}

get_NOT_FOUND <- function(topics){
  topics[is_function(topics) == -1]
}


#' Determines object type
#'
#' @param items items supplied from pkg_topics
#' @return 1 = is a function, 0 = is NOT a function, -1 = item is not found.
is_function <- function(items){
  sapply(items, function(i){
    item <- tryCatch(get(i), error = function(e){ "NOT_FOUND"})
#    if(is.character(item))
#      browser()
      
    if(is.character(item)){
      if(length(item)>0 && item[1] == "NOT_FOUND"){
        return(-1)
      }
    }
    
    if(strsplit(i,"")[[1]][1] == ".")
      return(2)
    
    is.function(item)
    
  })
}






