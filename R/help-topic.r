pkg_topic <- function(package, topic, file = NULL) {
  if (is.null(file)) {
    topics <- pkg_topics_index(package)
    print(topics)
    file <- topics$file[topics$alias == topic]
    file <- file[!is.na(file)]
    
    if(length(file) != 1)
      bprint(file)
    
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

# Function to turn a help topic into a convenient format.
parse_help <- function(rd) {
  tags <- sapply(rd, tag)

  # Remove top-level text strings - just line breaks between sections
  rd <- rd[tags != "TEXT"]

  out <- list()
  # Title, description, value and examples, need to be stitched into a 
  # single string.
  bprint(rd)
  out$title <- reconstruct(untag(rd$title))
  out$desc <- gsub("$\n+|\n+^", "", reconstruct(rd$description))
  out$details <- reconstruct(rd$details)
  out$value <- reconstruct(rd$value)
  out$examples <- highlight(reconstruct(untag(rd$examples)))
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
  arguments <- arguments[sapply(arguments, tag) != "TEXT"]
  out$params <- lapply(arguments, function(argument) {
    list(
      param = reconstruct(untag(argument[[1]])), 
      desc = reconstruct(untag(argument[[2]]))
    )
  })
  
  out
}

highlight <- function(examples) {
  if(identical(examples,"")) return(examples)
  if (!require(highlight)) return(examples)
  ex_parser <- parser(text = examples)
  str_join(capture.output(highlight::highlight( parser.output = ex_parser, renderer = highlight::renderer_html(doc = F))), collapse = "\n")    
}











