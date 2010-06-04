#' Tag Something
#'
#' @param x item to be tagged
tag <- function(x) attr(x, "Rd_tag")

#' Untag Something
#'
#' @param x item to be untagged
untag <- function(x) {
  if (is.null(x)) return()
  attr(x, "Rd_tag") <- ""
  x
}


all_tags <- function(rd) {
  if (is.null(rd)) return()
  if (!is.list(rd)) return(tag(rd))
  
  sort(unique(c(tag(rd), unlist(lapply(rd, all_tags)))))
}

reconstruct <- function(rd) {

  if (is.null(rd)) return("")
  if (!is.list(rd) && is.null(attr(rd, "Rd_tag"))) return(as.character(rd))
#  if (!is.list(rd)) return(as.character(rd))
  
  tag <- tag(rd)
  
  if (length(tag) == 0 || tag == "TEXT" || tag == "") {
    # Collapse text strings
#    str_trim(str_join(sapply(rd, reconstruct), collapse = ""))
    
    as.character(str_join(sapply(rd, reconstruct), collapse = ""))

  } else if (is_section(tag)) {
    # Sections should be arranged into paragraphs    
    text <- reconstruct(untag(rd))
    paras <- str_trim(str_split(text, "\\n\\n")[[1]])
    str_join("<p>", paras, "</p>", "\n\n", collapse = "")      
    
  } else if (tag %in% names(simple_tags)) {
    # If we can process tag with just prefix & suffix, do so
    tag_simple(tag, reconstruct(untag(rd)))

  } else if (tag == "\\link") {
    stopifnot(length(rd) == 1)
    fun <- rd[[1]]
    pkg <- attr(rd, "Rd_option")
    
    tag_link(fun, pkg)
  } else if (tag == "\\dontrun") {
    dr <- reconstruct(untag(rd))
    str_c("## Not run\n# ", str_replace(dr, "\n", "\n# "), "\n## ")
  } else if (tag == "\\eqn") {
#    rd[[1]]
    str_join("<code>", reconstruct(untag(rd)), "</code>")
  } else if (tag == "\\url") {
    stopifnot(length(rd) == 1)
    str_join("<a href='", rd[[1]], "'>", rd[[1]], "</a>")

  } else if(tag == "\\email"){
    str_join("<a href=\"mailto:",rd[[1]][1],"?subject=(R-Help): \">",reconstruct(untag(rd)),"</a>")
      
  } else if(tag == "COMMENT") {
    
    txt <- as.character(rd)
    str_replace(txt, "%", "#")
  } else if(tag == "\\enc"){
    
    reconstruct(rd[[1]])
    
  } else if(tag == "\\method") {
    
    str_join(reconstruct(rd[[1]]),".",reconstruct(rd[[2]]))

  } else {
    
    message("Unknown tag ", tag)
    browser()
    reconstruct(untag(rd))
  }
}

tag_simple <- function(tag, text) {
  stopifnot(length(text) == 1)
  html <- simple_tags[[tag]]
  str_join(html[1], text, html[2])
}

tag_link <- function(fun, pkg = NULL) {
  if (!is.null(pkg)) {
    str_join("<a href='/packages/", pkg, "/topics/", fun, "'>", fun, "</a>")        
  } else {
    str_join("<a href='", fun, "'>", fun, "</a>")
  }
}


is_section <- function(tag) {
  
  tag %in% c("\\details", "\\description", "\\value", "\\author", "\\seealso")
  
  
}

simple_tags <- list(
  "\\acronym" =      c('<acronym>','</acronym>'),
  "\\bold" =         c("<b>", "</b>"),
  "\\cite" =         c("<cite>", "</cite>"),
  "\\code" =         c("<code>", "</code>"),
  "\\command" =      c("<code>", "</code>"),
  "\\cr" =           c("<br >", ""),
  "\\describe" =     c("<span class=\"describe\">", "</span"),
  "\\dfn" =          c("<dfn>", "</dfn>"),
  "\\donttest" =     c("", ""),
  "\\dontshow" =     c("", ""),
  "\\dots" =         c("...", ""),
  "\\dquote" =       c("&ldquo;", "&rdquo;"),
  "\\dQuote" =       c("&ldquo;", "&rdquo;"),
  "\\emph" =         c("<em>", "</em>"),
  "\\enumerate" =    c("<ol>", "</ol>"),
  "\\env" =          c('<span class = "env">', '</span>'),
  "\\file" =         c('&lsquo;<span class = "file">', '</span>&rsquo;'),
  "\\item" =         c("<li>", ""),
  "\\itemize" =      c("<ul>", "</ul>"),
  "\\kbd" =          c("<kbd>", "</kbd>"),
  "\\ldots" =        c("...", ""),
  "\\option" =       c('<span class = "option">',"</span>"),
  "\\pkg" =          c('<span class = "pkg">',"</span>"),
  "\\preformatted" = c("<pre>","</pre>"),
  "\\R" =            c('<span style="R">R</span>', ""),
  "\\samp" =         c('<span class = "samp">',"</span>"),
  "\\sQuote" =       c("&lsquo;","&rsquo;"),
  "\\strong" =       c("<strong>", "</strong>"),
  "\\text" =         c("<p>", "</p>"),
  "\\var" =          c("<var>", "</var>"),
  "\\verb" =         c("<pre>", "</pre>"),

  "RCODE" =          c("", ""),
  "VERB" =           c("", ""),
  "LIST" =          c("<ul>", "</ul>")
)

has_length <- function(x){
    NROW(x) > 0 && x != ""
}


pluralize <- function(string, obj) {
  if(has_length(obj)) {
    str_join(string, "s", sep = "")
  } else {
    string
  }
}

strip_html <- function(x) {
  str_replace(x, "</?.*?>", "")
}
