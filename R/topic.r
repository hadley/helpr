topic <- function(package, topic) {
  topic_info <- parse_help(pkg_topic(package, topic))
  topic_info$package <- package

  topic_info
}

pkg_topic <- function(package, topic, file = NULL) {
  if (is.null(file)) {
    topics <- pkg_topics_index(package)
    file <- unique(topics$file[topics$alias == topic | topics$file == topic])
    
    stopifnot(length(file) == 1)    
  }
  
  name_rd(tools:::fetchRdDB(pkg_rddb_path(package), file))
}


name_rd <- function(rd) {
  tags <- sapply(rd, tag)
  tags <- gsub("\\\\", "", tags)
  names(rd) <- tags
  
  rd
} 

topic_is_internal <- function(help) {
  "internal" %in% help$keywords
}


# Function to turn a help topic into a convenient format.
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
  out$examples <- highlight(reconstruct(untag(rd$examples)))
  out$example_functions <- code_info(reconstruct(untag(rd$examples)))
  out$example_functions_str <- pluralize("Top Function", out$example_functions)
  out$usage <- reconstruct(untag(rd$usage))
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


highlight <- function(examples) {
  if(identical(examples,"") | identical(examples, "NULL")) return(examples)
  if (!require(highlight)) return(examples)
  
  # add links before being sent to be highlighted
  ex_parser <- add_function_links_into_parsed(parser(text = examples))
  
  str_join(capture.output(highlight::highlight( parser.output = ex_parser, renderer = highlight::renderer_html(doc = F))), collapse = "\n")    
}


add_function_links_into_parsed <- function(ex_parser){
  # pull out data
  d <- attr(ex_parser, "data")
  
#  funcs <- d[d[,"token.desc"] == "SYMBOL_FUNCTION_CALL" ,"text"]
  rows <- with(d, (token.desc == "SYMBOL_FUNCTION_CALL" & ! text %in% c("", "(",")") ) | text %in% c("UseMethod"))
  funcs <- d[rows,"text"]

  # make links for functions and not for non-package functions
  links <- function_and_link(str_join(funcs, "()"), complete = FALSE)
  text <- str_join("<a href='", links$paths, "' >", links$functions,"</a>")
  text[is.na(links$paths)] <- links$functions[is.na(links$paths)]
  
  # return data
  d[rows,"text"] <- text

#  d[d[,"token.desc"] == "SYMBOL_FUNCTION_CALL","text"] <- text
  attr(ex_parser, "data") <- d
  ex_parser
  
}


first_item_pos <- function(arr){
  for(i in seq_along(arr))
    if(arr[i] == "\\item")
      return(i)
  1
}

last_item_pos <- function(arr){
  for(i in rev(seq_along(arr)))
    if(arr[i] == "\\item")
      return(i)
  0  
}