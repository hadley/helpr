#' Work out the source code of a function.
body_text <- function(fun) {
  str_c(deparse(body(fun)), collapse = "\n")
}

#' Count how many times a function calls other functions.
# 
#' @param fun string giving name of function, or function
#' @export
function_calls <- function(fun) {
#  text <- body_text(fun)
#  
#  pieces <- attr(parser(text = text), "data")
#  calls <- subset(pieces, token.desc == "SYMBOL_FUNCTION_CALL")$text
#  
#  as.data.frame(table(fun = calls), responseName = "freq",
#    stringsAsFactors = FALSE)
  src_function_count(body_text(fun))

}


#' Return the package functions and links of a given text
#'
#' @param text text to be parsed
function_and_link <- function(text, complete = TRUE){
  
  parsed_funcs <- as.data.frame(attributes(parser(text = text))$data, stringsAsFactors = FALSE)
  functions <- subset(parsed_funcs, token.desc == "SYMBOL_FUNCTION_CALL")$text
  
  paths <- function_help_path(functions)
  
  funcs_and_paths <- as.data.frame(list(functions = functions, paths = paths), stringsAsFactors = FALSE)

  if(complete)
    funcs_and_paths[complete.cases(funcs_and_paths),]  
  else
    funcs_and_paths
}

#' Return the help path of a function
#'
#' @param func function to find the help path
function_help_path <- function(func){
  sapply(func, function(x){
    
    tmp <- help(x)[1] 
    if(is.na(tmp)){
      NA
    }else{
      # retrieve last three folders/file and keep the package and topic
      pack_and_topic <- rev(rev(str_split(tmp, .Platform$file.sep)[[1]])[1:3])[c(1,3)]
      str_join("/packages/",pack_and_topic[1], "/topics/", pack_and_topic[2])
    }
  })  
}


#' Find functions, counts, and links of given R text
#'
#' @param text text to be parsed
src_function_count <- function(text){
  if(is.null(text) || text == "" || text == "NULL")
    return(list())
  
  funcs_and_paths <- function_and_link(as.character(text))
  funcs <- table(funcs_and_paths$functions)
  order <- order(funcs, decreasing = TRUE)

  uni_funs <- unique(funcs_and_paths)
#  uni_funs <- uni_funs[, ]

  # order the outputs the same
  funcs <- funcs[order]
  # make alphabetical to match table output
  uni_funs <- uni_funs[order(uni_funs$functions)[order], ]

  list(
    name = uni_funs$functions,
    count = funcs,
    link = uni_funs$paths,
    str = pluralize("Top Function", funcs)
  )
}


function_src <- function(package, func){
  
  index <- pkg_topics_index(package)
  topic <- index[index$alias == func, "file"]
  
  list(
    package = package, 
    name = func,
    aliases = topic_and_alias(package, topic, omit = func),
    desc = topic(package, topic)$desc,
    src = highlight(body_text(func)),
    src_functions = function_calls(func)
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
