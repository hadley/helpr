#' Funcition Source
#' Work out the source code of a function.
#'
#' @param fun function to get the source code from
#' @return NULL or source code of of the function
body_text <- function(fun) {
  text <- get(fun, mode = "function")
  if(is.null(text))
    NULL
  else
    str_c(deparse(text), collapse = "\n")
}


#' Return the package functions and links of a given text
#'
#' @param parser_output text that has been parsed
function_and_link <- function(parser_output){
  
  parsed_funcs <- as.data.frame(attr(parser_output, "data"), stringsAsFactors = FALSE)
  functions <- subset(parsed_funcs, token.desc %in% c("SYMBOL_FUNCTION_CALL", "NULL_CONST"))$text
  
  paths <- function_help_path(functions)
  
  funcs_and_paths <- as.data.frame(list(functions = functions, paths = paths), stringsAsFactors = FALSE)

  funcs_and_paths[complete.cases(funcs_and_paths),]  
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
#' @param parser_output text that has been parsed
#' @return data.frame containing the name, count and link of each function within the text
code_info <- function(parser_output){
  if(is.null(parser_output))
    return(data.frame())

  funcs_and_paths <- function_and_link(parser_output)
  if(!dataframe_has_rows(funcs_and_paths))
    return(data.frame())
  
  funcs <- table(funcs_and_paths$functions)
  order <- order(funcs, decreasing = TRUE)

  uni_funs <- unique(funcs_and_paths)

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


helpr_function <- function(package, func){
  
  index <- pkg_topics_index(package)
  topic <- index[index$alias == func, "file"]
  par_text <- reconstruct(body_text(func))
  src_frunctions <- code_info(par_text)
  list(
    package = package, 
    name = func,
    aliases = topic_and_alias(package, topic, omit = func),
    desc = topic(package, topic)$desc,
    src = highlight(par_text),
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
