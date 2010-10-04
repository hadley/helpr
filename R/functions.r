#' Extract source code of a function.
#'
#' @param fun name of function to get the source code from
#' @return NULL or source code of of the function
#' @author Barret Schloerke \email{schloerke@@gmail.con} and Hadley Wickham
#' @keywords internal
body_text <- function(package, fun) {
  text <- tryCatch(
    get(fun, mode = "function"),
    error = function(e) {
      tryCatch(
        get(fun, mode = "function", envir = asNamespace(package)),
        error = function(e)
          stop("can't find the function ", fun)
      )
    }
  )
    
  if (is.null(text)) {
    NULL
  } else {
    str_c(deparse(text), collapse = "\n")
  }
}


#' Return the package functions and links of a given text.
#'
#' @param parser_output text that has been parsed
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
function_and_link <- function(parser_output) {
  parsed_funcs <- as.data.frame(attr(parser_output, "data"), stringsAsFactors = FALSE)
  functions <- subset(parsed_funcs, token.desc %in% c("SYMBOL_FUNCTION_CALL", "NULL_CONST"))$text
  
  paths <- function_help_path(functions)
  
  funcs_and_paths <- as.data.frame(list(functions = functions, paths = paths), stringsAsFactors = FALSE)

  funcs_and_paths[complete.cases(funcs_and_paths), ]  
}

#' Retrieve the package and the topic from a url that contains both.
#'
#' @param url_string url in question
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
pkg_and_topic_from_help_url <- function(url_string) {
  rev(rev(str_split(url_string, .Platform$file.sep)[[1]])[1:3])[c(1, 3)]
}

#' Return the help path of a function.
#'
#' @param x item to find the help path
#' @param source_link boolean to determine whether or not it is linking to the source or topic page
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
#' @aliases function_help_path function_help_path_mem
function_help_path_mem <- memoise(function(x, source_link = FALSE) {
  url_string <- help(x)[1] 
  if (is.na(url_string)) {
    NA
  } else {
    # retrieve last three folders/file and keep the package and topic
    pack_and_topic <- pkg_and_topic_from_help_url(url_string)

    ending <- str_c("/topic/", x)
    if (source_link)
      ending <- str_c(ending, "/source")
    str_c("/package/", pack_and_topic[1], ending)
  }
})

function_help_path <- function(func, source_link = FALSE) {
  sapply(func, function_help_path_mem, source_link = source_link)
}


#' Find functions, counts, and links of given R text.
#'
#' @param parser_output text that has been parsed
#' @return data.frame containing the name, count and link of each function
#'   within the text
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
code_info <- function(parser_output) {
  if (is.null(parser_output))
    return(data.frame())

  funcs_and_paths <- function_and_link(parser_output)
  if (!dataframe_has_rows(funcs_and_paths))
    return(data.frame())
  
  funcs <- table(funcs_and_paths$functions)
  order <- order(funcs, decreasing = TRUE)

  uni_funs <- unique(funcs_and_paths)

  # order the outputs the same
  funcs <- funcs[order]

  # make alphabetical to match table output
  uni_funs <- uni_funs[order(uni_funs$functions)[order], ]

  name_count_link <- data.frame(
    name = uni_funs$functions,
    count = funcs,
    link = uni_funs$paths
  )
  
  name_count_link <- subset(name_count_link, count > 1)
  
  rows <- NROW(name_count_link)
  if(rows > 10) {
    rows <- 10
  } else if(rows > 5) {
    rows <- 5
  }
  
  name_count_link[seq_len(rows), ]
}


#' Render all the information to display a topic source page.
#'
#' @param package package in question
#' @param func function in question
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
helpr_function <- function(package, func) {
  
  index <- pkg_topics_index(package)
  topic <- as.character(subset(index, alias == func, "file"))
  aliases <- subset(index, (file == topic) & (alias != func), "alias")
  
  par_text <- tryCatch(
    parse_text(reconstruct(body_text(package, func), package)),
    error = function(e){
      "bad_function"
    }
  )
  
  if (identical(par_text, "bad_function")) {
    
    input <- str_c("str(", func, ")", collapse = "")
    src <- capture.output(eval(parser(text = input)[1]))
    src <- str_replace_all(src, "<", "&lt;")
    src <- str_replace_all(src, ">", "&gt;")
    src <- eval_tag_output(str_c(src, collapse = "\n"))
    src <- str_c("<pre>", input,"</pre>", src)
    src_functions <- NULL
    src_functions_str <- ""
    
  } else {
    src_functions <- code_info(par_text)
    src <- highlight(par_text)
    src_functions_str <- pluralize("Top Function", src_functions)
  }
  
  list(
    package = package, 
    topic = topic,
    name = func,
    aliases = aliases,
    aliases_str = pluralize("Topic (Source)", aliases, plural="Topics (Source)"),
    desc = gsub("$\n+|\n+^", "", reconstruct(pkg_topic(package, topic)$description, package)),
    src = src,
    src_functions = src_functions,
    src_functions_str = src_functions_str,
    change_log = function_news(package, topic),
    topic_in_example = solr_has_topic_in_example(topic)
  )
}




