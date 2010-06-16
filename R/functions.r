#' Work out the source code of a function.
body_text <- function(fun) {
  text <- get(fun, mode = "function")
  if(is.null(text))
    NULL
  else
    str_c(deparse(text), collapse = "\n")
}


#' Return the package functions and links of a given text
#'
#' @param text text to be parsed
function_and_link <- function(text, complete = TRUE){
  parsed_funcs <- as.data.frame(attributes(parser(text = text))$data, stringsAsFactors = FALSE)
  functions <- subset(parsed_funcs, token.desc %in% c("SYMBOL_FUNCTION_CALL", "NULL_CONST"))$text
  
  paths <- function_help_path(functions)
  
  funcs_and_paths <- as.data.frame(list(functions = functions, paths = paths), stringsAsFactors = FALSE)

  if(complete)
    funcs_and_paths[complete.cases(funcs_and_paths),]  
  else
    funcs_and_paths
}

#' Return the help path of a function
#'
#' @param x item to find the help path
function_help_path_mem <- memoise(function(x){
  tmp <- help(x)[1] 
  if(is.na(tmp)){
    NA
  }else{
    # retrieve last three folders/file and keep the package and topic
    pack_and_topic <- rev(rev(str_split(tmp, .Platform$file.sep)[[1]])[1:3])[c(1,3)]
    str_join("/packages/",pack_and_topic[1], "/topics/", pack_and_topic[2])
  }
})

function_help_path <- function(func){
  sapply(func, function_help_path_mem)
}




#' Find functions, counts, and links of given R text
#'
#' @param text text to be parsed
code_info <- function(text){
  if(is.null(text) || str_trim(text) == "")
    return(list())
  funcs_and_paths <- function_and_link(as.character(text))
  if(!has_length(funcs_and_paths))
    return(list())
  
  funcs <- table(funcs_and_paths$functions)
  order <- order(funcs, decreasing = TRUE)

  uni_funs <- unique(funcs_and_paths)
#  uni_funs <- uni_funs[, ]

  # order the outputs the same
  funcs <- funcs[order]
  # make alphabetical to match table output
  uni_funs <- uni_funs[order(uni_funs$functions)[order], ]

  data.frame(
    name = uni_funs$functions,
    count = funcs,
    link = uni_funs$paths
  )
}


function_info <- function(package, func){
  
  index <- pkg_topics_index(package)
  topic <- index[index$alias == func, "file"]
  src_frunctions <- code_info(body_text(func))
  list(
    package = package, 
    name = func,
    aliases = topic_and_alias(package, topic, omit = func),
    desc = topic(package, topic)$desc,
    src = highlight(reconstruct(body_text(func))),
    src_functions = src_frunctions,
    src_functions_str = pluralize("Top Function", src_frunctions)
  )
}


topic_and_alias <- function(package, topic, omit = ""){
  index <- pkg_topics_index(package)
  index <- index[topic == index$file, ]
  aliases <- index[index$alias != omit, "alias"]
  list(
    alias = aliases,
    str = pluralize("Alias (Source)", aliases, plural="Aliases (Source)")
  )
}
