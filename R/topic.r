#' Helpr Home
#'
#' @return all the information necessary to produce the home site ("index.html")
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
helpr_topic <- function(package, topic, highlight) {
  topic_info <- parse_help(pkg_topic(package, topic), package = package)
  topic_info$package <- package
  topic_info$highlight <- highlight
  
  topic_info$similar <- solr_similar(topic_info$title)

  topic_info
}

#' Package Topic R Documentation
#'
#' @param package package to explore
#' @param topic topic of the package to retrieve
#' @param file location of the rd database.  If it is \code{NULL}, it will be found.
#' @author Haldey Wickham
#' @keywords internal
#' @return text of the .rd file
pkg_topic <- function(package, topic, file = NULL) {
  if (is.null(file)) {
    topics <- pkg_topics_index(package)
    file <- unique(topics$file[topics$alias == topic | topics$file == topic])
    
    if(length(file) > 1)
      file <- unique(topics$file[topics$alias == topic])
    
    stopifnot(length(file) == 1)    
  }
  
  name_rd(tools:::fetchRdDB(pkg_rddb_path(package), file))
}


#' Name R Documentation
#'
#' @param rd rd file to use
#' @return rd file properly named according to the tags
#' @author Hadley Wickham
#' @keywords internal
name_rd <- function(rd) {
  tags <- sapply(rd, tag)
  tags <- gsub("\\\\", "", tags)
  names(rd) <- tags
  
  rd
} 

#' Internal Topic Function
#'
#' @param help \code{pkg_topic(}\emph{\code{topic}}\code{)}  is checked to see if a keyword is "internal"
#' @return boolean
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
topic_is_internal <- function(help) {
  "internal" %in% help$keywords
}

#' Parse Help File
#' Function to turn a help topic into a convenient format.
#'
#' @param rd item to be tagged recursively
#' @return item reformatted to be used in HTML
#' @author Hadley Wickham and Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
parse_help <- function(rd, package) {
  tags <- sapply(rd, tag)

  # Remove top-level text strings - just line breaks between sections
  rd <- rd[tags != "TEXT"]

  out <- list()
  
  # Join together aliases and keywords
  out$name <- reconstruct(untag(rd$name))
  out$aliases <- setdiff(
    unname(sapply(rd[names(rd) == "alias"], "[[", 1)),
    out$name
  )
  out$keywords <- unname(sapply(rd[names(rd) == "keyword"], "[[", 1))

  # Title, description, value and examples, need to be stitched into a 
  # single string.
  out$title <- reconstruct(untag(rd$title))
  out$desc <- gsub("$\n+|\n+^", "", reconstruct(rd$description))
  out$details <- reconstruct(rd$details)
  out$value <- reconstruct(rd$value)
  reconstructed_examples <- reconstruct(untag(rd$examples))
  par_text <- parse_text(reconstructed_examples)
  out$examples <- highlight(par_text)
  out$example_functions <- code_info(par_text)
  out$example_functions_str <- pluralize("Top Function", out$example_functions)
#  out$usage <- reconstruct(untag(rd$usage))
  out$usage <- parse_usage(rd$usage)
  out$authors <- pkg_author_and_maintainers(reconstruct(rd$author))
  out$author_str <- pluralize("Author", rd$author)

  out$seealso <- reconstruct(rd$seealso)
  out$source <- reconstruct(untag(rd$source))
  

  # Pull apart arguments
  arguments <- rd$arguments
#  arguments <- arguments[! sapply(arguments, tag) %in% c("TEXT", "COMMENT")]
  argument_tags <- sapply(arguments, tag)
  args <- lapply(arguments[argument_tags == "\\item"], function(argument) {
    list(
      param = reconstruct(untag(argument[[1]])), 
      desc = reconstruct(untag(argument[[2]]))
    )
  })
  
  pre_text <- reconstruct(arguments[ seq_len( first_item_pos( argument_tags) - 1)])
  
  post_text <- reconstruct(
    arguments[seq(
      from = last_item_pos(argument_tags)+1, 
      length.out = length(arguments) - last_item_pos(argument_tags)
    )]
  )

  out$params <- list(
    args = args,
    pre_text = pre_text,
    post_text = post_text
  )

  out
}


#' Highlight R Text
#' Highlights R text to include links to all functions and make it easier to read
#' @param parser_output text to be parsed and highlighted
#' @return highlighted text
#' @author Hadley Wickham and Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
highlight <- function(parser_output, source_link = FALSE) {
  if(is.null(parser_output))
    return("")
  
  # add links before being sent to be highlighted
  parser_output <- add_function_links_into_parsed(parser_output, source_link)  
  
  str_c(capture.output(highlight::highlight( parser.output = parser_output, renderer = highlight::renderer_html(doc = F))), collapse = "\n")    
}

#' Add Funciton Link
#' Add the function link to the preparsed R code
#'
#' @param parser_output pre-parsed output
#' @return parsed output with functions with html links around them
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
add_function_links_into_parsed <- function(parser_output, source_link = FALSE){
  # pull out data
  d <- attr(parser_output, "data")
  
#  funcs <- d[d[,"token.desc"] == "SYMBOL_FUNCTION_CALL" ,"text"]
  rows <- with(d, (token.desc == "SYMBOL_FUNCTION_CALL" & ! text %in% c("", "(",")") ) | text %in% c("UseMethod"))

  if(!TRUE %in% rows)
    return(parser_output)
    
  funcs <- d[rows,"text"]

  # make links for functions and not for non-package functions
  paths <- function_help_path(funcs, source_link)  
  text <- str_c("<a href='", paths, "'>", funcs,"</a>")
  text[is.na(paths)] <- funcs[is.na(paths)]
  
  # return data
  d[rows,"text"] <- text

#  d[d[,"token.desc"] == "SYMBOL_FUNCTION_CALL","text"] <- text
  attr(parser_output, "data") <- d
  parser_output
  
}

#' Find the First Item Position
#' @param arr arr of items to look at
#' @return position of the first item to match "\\item" else 1
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
first_item_pos <- function(arr){
  for(i in seq_along(arr))
    if(arr[i] == "\\item")
      return(i)
  1
}

#' Find the Last Item Position
#' @param arr arr of items to look at
#' @return position of the last item to match "\\item" else 0
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
last_item_pos <- function(arr){
  for(i in rev(seq_along(arr)))
    if(arr[i] == "\\item")
      return(i)
  0  
}


#' Parse Usage
#' Parse the topic usage to add links to functions
#'
#' @param usage rd usage
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
parse_usage <- function(usage){
  
  text <- reconstruct(untag(usage))
  
  text_lines <- str_split(text, "\n")[[1]]
  text_lines <- text_lines[ nchar(text_lines) > 1]
  text_lines <- text_lines[ str_sub(text_lines, end = 1) != " " ]
  
  
  pattern <- "[a-zA-Z_.][a-zA-Z_.0-9]*[ ]*\\("
    
#  alias_funcs <- unlist(str_extract(text_lines, pattern))
#  alias_funcs <- str_trim(str_replace_all(alias_funcs, "\\(", ""))
  alias_funcs <- usage_functions(text)
  
  funcs_text <- unlist(str_extract_all(text, pattern))
  funcs <- str_replace_all(funcs_text, "\\(", "")
  funcs <- safely_order_funcs(funcs)
  original_funcs <- funcs
  funcs <- str_trim(funcs)
  
  # add links to each "safely ordered" function
  for(i in seq_along(funcs)){
    func <- funcs[i]
    ori_func <- original_funcs[i]
    
    path <- function_help_path(func, source_link = (func %in% alias_funcs))
    
    if(is.na(path)) {
      link <- str_c("<em>",ori_func, "</em>(")
    } else {
      spaces <- str_c(rep(" ", nchar(ori_func) - nchar(func)), collapse = "")
      link <- str_c("<a href=\"", path, "\">", func, "</a>", spaces ,"(" )
    }
    
    text <- str_replace_all(text, str_c(ori_func,"\\("), link)
  }
  
  # add links to all the inner functions to their own help pages
  text
}

#' Order Functions Safely by Name
#'
#' @param vect string vector to be processed
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
safely_order_funcs <- function(vect){
  
  # add a ending string to only allow for end of string comparisons
  vect <- str_c(vect, "_helpr")
  
  # search from i in 1:n; j in i+1:n
  len <- length(vect)
  for(i in seq_len(len)){
    for(j in (seq_len(len - i) + i)){
      if(str_detect(vect[j], vect[i])){
        tmp <- vect[j]
        vect[j] <- vect[i]
        vect[i] <- tmp
      }
    }
  }
  str_replace_all(vect, "_helpr", "")
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


