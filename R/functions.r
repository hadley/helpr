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
function_help_path_mem <- memoise(function(x, source_link = FALSE){
  tmp <- help(x)[1] 
  if(is.na(tmp)){
    NA
  }else{
    # retrieve last three folders/file and keep the package and topic
    pack_and_topic <- rev(rev(str_split(tmp, .Platform$file.sep)[[1]])[1:3])[c(1,3)]

    ending <- str_join("/topic/", pack_and_topic[2])
    if(source_link)
      ending <- str_join(ending, "/source/", x)
    str_join("/package/",pack_and_topic[1], ending)
  }
})

function_help_path <- function(func, source_link = FALSE){
  sapply(func, function_help_path_mem, source_link = source_link)
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
  topic <- as.character(subset(index, alias == func, "file"))
  aliases <- subset(index, (file == topic) & (alias != func), "alias")
  par_text <- parse_text(reconstruct(body_text(func)))
  src_frunctions <- code_info(par_text)
  
  list(
    package = package, 
    topic = topic,
    name = func,
    aliases = aliases,
    aliases_str = pluralize("Alias (Source)", aliases, plural="Aliases (Source)"),
    desc = helpr_topic(package, topic)$desc,
    src = highlight(par_text),
    src_functions = src_frunctions,
    src_functions_str = pluralize("Top Function", src_frunctions)
  )
}


topic_and_alias <- function(package, topic, omit = ""){
  index <- pkg_topics_index(package)
  index <- index[topic == index$file, ]
  index[index$alias != omit, "alias"]
}
