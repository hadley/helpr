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

#  vignettes <- vignette(package= package)$results[, 3]
#  paths <- sapply(vignettes, function(v) vignette(v, package = package)$file)   
#
#  paths <- sapply(paths, function(i){
#    if(str_sub(i, str_length(i)-3, str_length(i)) != ".pdf"){
#      str_c(str_sub(i, end = str_length(i)-4), ".pdf")
#    }else{
#      i
#    }
#  })
  
#  http://cran.r-project.org/web/packages/roxygen/vignettes/roxygen.pdf
  titles <- vignette(package = package)$results[,4]
  pdfNames <- vignette(package= package)$results[, 3]
  
  titles <- str_replace(titles, "source, pdf", "")
  titles <- str_trim(str_replace(titles, "[()]", ""))
  
  paths <- str_join("http://cran.r-project.org/web/packages/", package, "/vignettes/", pdfNames, ".pdf", sep = "")
  
  list(titles = titles, paths = paths, vig_str = pluralize("Vignette", paths))
}

crantastic_rating <- function(pkg_name){
  txt <- RCurl::getURL(str_join("http://crantastic.org/packages/",pkg_name,"/"))
  rating_location <- str_locate(txt, "overall-rating")[,"start"]

  # retrieve the characters of overall-rating (and some extra)
  subString <- str_sub(txt, start = rating_location, end = rating_location+70)
  
  str_join(str_trim(str_split(subString, "\n")[[1]][2:3]), collapse = "<br />")  
}

rating_text <- function(pkg_name){
  rating_txt <- crantastic_rating(pkg_name)

  # get start position
  rating_value <- as.numeric(str_split(rating_txt,"/")[[1]][1])
  
  star <- "<input name=\"star_rating\" type=\"radio\" class=\"star\" disabled=\"disabled\"/>"
  checked_star <- "<input name=\"star_rating\" type=\"radio\" class=\"star\" disabled=\"disabled\" checked=\"checked\"/>"

  stars_txt <- c(
    rep(star, rating_value - 1),
    checked_star,
    rep(star, 5 - rating_value)
  )
  
  stars_txt <- str_join(stars_txt, collapse = "" )
  cran_txt <- str_join("http://crantastic.org/packages/", pkg_name)
  link_start <- str_join("<a href='",cran_txt,"' target=\"_blank\">")
  
  str_join(link_start, rating_txt, "</a>", "<br />", stars_txt)
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
  if(is.list(x)){
    !is.null(dim(x)) && dim(x)[1] > 0
  }
  else{
    length(x) > 0 && x != ""
  }
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






