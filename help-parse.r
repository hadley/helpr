library(highlight)
source("rd-tags.r")

tag <- function(x) attr(x, "Rd_tag")

untag <- function(x) {
  if (is.null(x)) return()
  attr(x, "Rd_tag") <- ""
  x
}

reconstruct <- function(rd) {
  if (is.null(rd)) return("")
  
  if (is.list(rd)) {
    # Special tags get ignored
    special <- tag(rd) == toupper(tag(rd))
    
    tag <- tolower(gsub("\\\\", "", tag(rd)))
    
    if (tag == "link") {
      fun <- rd[[1]]
      pkg <- attr(rd, "Rd_option")
      if (!is.null(pkg)) {
        out <- str_join("<a href='/packages/", pkg, "/topics/", fun, "'>", fun, "</a>")        
      } else {
        out <- str_join("<a href='", fun, "'>", fun, "</a>")
      }
    } else if (tag == "url") {
      out <- str_join("<a href='", rd[[1]], "'>", rd[[1]], "</a>")
    } else {
      html <- html_tags[tag]

      prefix <- ifelse(special, "", sapply(html, "[", 1))
      suffix <- ifelse(special, "", sapply(html, "[", 2))
      out <- paste(
        prefix, paste(sapply(rd, reconstruct), collapse = ""), suffix, 
       sep = "")      
    }
    
    if (tag(rd) == "TEXT") {
      out <- str_split(out, "\\n\\n")[[1]]
      out <- str_join("<p>", str_trim(out), "</p>", "\n\n", collapse = "")
    } 
    str_trim(out)
  } else {
    rd
  }  
}

# Function to turn a help topic into a convenient format.
parse_help <- function(rd) {
  tags <- sapply(rd, tag)
  tags <- gsub("\\\\", "", tags)
  names(rd) <- tags

  # Remove top-level text strings - just line breaks between sections
  rd <- rd[tags != "TEXT"]

  out <- list()
  # Title, description, value and examples, need to be stitched into a 
  # single string.
  out$title <- reconstruct(untag(rd$title))
  out$desc <- gsub("$\n+|\n+^", "", reconstruct(untag(rd$description)))
  out$details <- reconstruct(untag(rd$details))
  out$value <- reconstruct(untag(rd$value))
  out$examples <- highlight(reconstruct(untag(rd$examples)))
  out$usage <- reconstruct(untag(rd$usage))
  out$authors <- reconstruct(untag(rd$author))

  out$seealso <- reconstruct(untag(rd$seealso))
  
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
  ex_parser <- parser(text = examples)
  str_join(capture.output(highlight::highlight( parser.output = ex_parser, renderer = renderer_html(doc = F))), collapse = "\n")
}