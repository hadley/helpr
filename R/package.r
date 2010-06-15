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

pkg_demos <- function(package, omit = "") {
#  demos <- demo(package = package)$results
#  files <- apply(demos, 1, function(dem){ 
#   pkg_demo_src_file(dem[ "Package"], dem[ "Item"])
#  })

  dems <- demo(package = package)$results
  dems <- dems[dems[,"Item"] != omit, ]
  
  
  if(!has_length(dems))
    NULL
  else
    list(
      dems = dems, 
      demo_str = pluralize("Demo", dems)
    )
  
}

#pkg_demo_src_file <- function(package, dem) {
#  system.file("demo", dem, package = package)
#}

exec_pkg_demo <- function(package, dem) {
#  demo(dem, character = TRUE, package = package, ask = TRUE)
  demo(dem, character = TRUE, package = package, ask = FALSE)
}

pkg_vigs <- function(package) {
  vignettes <- vignette(package = package)$results
  
  if(!has_length(vignettes))
    return(NULL)

  titles <- vignettes[,4]
  pdfNames <- vignettes[, 3]
  paths <- vignettes[, 2]
  
  titles <- str_replace(titles, "source, pdf", "")
  titles <- str_trim(str_replace(titles, "[()]", ""))
  
#    paths <- str_join("http://cran.r-project.org/web/packages/", package, "/vignettes/", pdfNames, ".pdf", sep = "")
    paths <- str_join("/_",str_join(paths, package, "doc", str_join(pdfNames, ".pdf"), sep = .Platform$file.sep))
  
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
  
  file_path <- file.path(help_path, "AnIndex")
  ### check to see if there is anything that exists, aka sinartra
  if(!has_length(readLines(file_path, n = 1)))
    return(NULL)

  topics <- read.table(file_path, sep = "\t", 
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

data_set_names <- function(name){
  str_extract_all(name, "[a-zA-Z_.][a-zA-Z_.0-9]*")[[1]]
}

get_datasets <- function(package){
#  topics[is_function(topics) == 0]
  sets <- suppressWarnings(data(package = package)$results)
  
  # make sure it gets the right package (base and datasets)
  items <- sets[sets[,"Package"] == package, "Item"]
  
  # extract names of datasets that belong to a group, 
  #   i.e. beaver1 (beaver) becomes c("beaver1", "beaver")
  data_sets <- as.character(unique(unlist(sapply(items, data_set_names))))
  data_sets[order(data_sets)]
}


get_pkg_topic_type <- function(package){
  types <- pkg_topic_index_type(package)
  
  list(
    func = types[types$type == "function", 1:2],
    dataset = types[types$type == "dataset", 1:2],
    internal = types[types$type == "internal", 1:2],
    idk = types[types$type == "IDK", 1:2],
    package = types[types$type == "package", 1:2]
  )
}


#' Determines object type
#'
#' @param items items supplied from pkg_topics
#' @return boolean
is_function <- function(items){
  exists(items, mode="function")
}

pkg_topic_index_type <- function(package){
  index <- pkg_topics_index(package)
  
  index$type <- "IDK"
  
  # dataset
  rows <- 
    index$file %in% get_datasets(package) |
    index$alias %in% get_datasets(package)
  
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

