#' Helpr Home
#'
#' @return all the information necessary to produce the home site ("index.html")
helpr_topic <- function(package, topic) {
  topic_info <- parse_help(pkg_topic(package, topic))
  topic_info$package <- package

  topic_info
}

#' Package Topic R Documentation
#'
#' @param package package to explore
#' @param topic topic of the package to retrieve
#' @param file location of the rd database.  If it is \code{NULL}, it will be found.
#'
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
name_rd <- function(rd) {
  tags <- sapply(rd, tag)
  tags <- gsub("\\\\", "", tags)
  names(rd) <- tags
  
  rd
} 

#' Internal Topic Function
#'
#' @param help \code{\link{pkg_topic(\emph{topic})}}  is checked to see if a keyword is "internal"
#' @return boolean
topic_is_internal <- function(help) {
  "internal" %in% help$keywords
}

#' Parse Help File
#' Function to turn a help topic into a convenient format.
#'
#' @param rd item to be tagged recursively
#' @return item reformatted to be used in HTML
parse_help <- function(rd) {
  tags <- sapply(rd, tag)

  # Remove top-level text strings - just line breaks between sections
  rd <- rd[tags != "TEXT"]

  out <- list()
  # Title, description, value and examples, need to be stitched into a 
  # single string.
  out$title <- reconstruct(untag(rd$title))
  out$desc <- gsub("$\n+|\n+^", "", reconstruct(rd$description))
  out$details <- reconstruct(rd$details)
  out$value <- reconstruct(rd$value)
  par_text <- parse_text(reconstruct(untag(rd$examples)))
  out$examples <- highlight(par_text)
  out$example_functions <- code_info(par_text)
  out$example_functions_str <- pluralize("Top Function", out$example_functions)
#  out$usage <- reconstruct(untag(rd$usage))
  out$usage <- highlight(parse_text(reconstruct(untag(rd$usage))), source_link = TRUE)
  out$authors <- reconstruct(rd$author)
  out$author_str <- pluralize("Author", rd$author)

  out$seealso <- reconstruct(rd$seealso)
  
  # Join together aliases and keywords
  out$name <- reconstruct(untag(rd$name))
  out$aliases <- setdiff(
    unname(sapply(rd[names(rd) == "alias"], "[[", 1)),
    out$name
  )
  out$keywords <- unname(sapply(rd[names(rd) == "keyword"], "[[", 1))

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
highlight <- function(parser_output, source_link = FALSE) {
  if(is.null(parser_output))
    return("")
  
  # add links before being sent to be highlighted
  parser_output <- add_function_links_into_parsed(parser_output, source_link)  
  
  str_join(capture.output(highlight::highlight( parser.output = parser_output, renderer = highlight::renderer_html(doc = F))), collapse = "\n")    
}

#' Add Funciton Link
#' Add the function link to the preparsed R code
#'
#' @param parser_output pre-parsed output
#' @return parsed output with functions with html links around them
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
  text <- str_join("<a href='", paths, "'>", funcs,"</a>")
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
# '
first_item_pos <- function(arr){
  for(i in seq_along(arr))
    if(arr[i] == "\\item")
      return(i)
  1
}

#' Find the Last Item Position
#' @param arr arr of items to look at
#' @return position of the last item to match "\\item" else 0
last_item_pos <- function(arr){
  for(i in rev(seq_along(arr)))
    if(arr[i] == "\\item")
      return(i)
  0  
}