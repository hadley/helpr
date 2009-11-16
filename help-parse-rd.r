rd <- pkg_topic("stats", "glm")
tags <- sapply(rd, tag)
tags <- gsub("\\\\", "", tags)
names(rd) <- tags


tag <- function(x) attr(x, "Rd_tag")

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
  if (is.null(rd)) return()
  
  tag <- tag(rd)

  if (length(tag) == 0 || tag %in% c("", "TEXT")) {
    # Top level with no tag, just recurse
    if (is.character(rd)) return(as.character(rd))

    pieces <- sapply(rd, reconstruct)
    text <- str_join(pieces, collapse = "")

    if (length(tag) > 0 && tag == "") return(text)
    
    str_join("<p>", str_split(text, "\\n\\n")[[1]], "</p>", "\n\n", 
      collapse = "")      

  } else if (tag %in% names(simple_tags)) {
    # If we can process tag with just prefix & suffix, do so
    tag_simple(tag, reconstruct(untag(rd)))

  } else if (tag == "\\link") {
    stopifnot(length(rd) == 1)
    fun <- rd[[1]]
    pkg <- attr(rd, "Rd_option")
    
    tag_link(fun, pkg)

  } else if (tag == "\\url") {
    stopifnot(length(rd) == 1)
    str_join("<a href='", rd[[1]], "'>", rd[[1]], "</a>")

  } else {
    message("Unknown tag ", tag)
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


simple_tags <- list(
  "\\acronym" =      c('<acronym>','</acronym>'),
  "\\bold" =         c("<b>", "</b>"),
  "\\cite" =         c("<cite>", "</cite>"),
  "\\code" =         c("<code>", "</code>"),
  "\\command" =      c("<code>", "</code>"),
  "\\cr" =           c("<br >", ""),
  "\\dfn" =          c("<dfn>", "</dfn>"),
  "\\donttest" =     c("", ""),
  "\\dots" =         c("...", ""),
  "\\dquote" =       c("&ldquo;", "&rdquo;"),
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
  "\\squote" =       c("&lsquo;","&rsquo;"),
  "\\strong" =       c("<strong>", "</strong>"),
  "\\text" =         c("<p>", "</p>"),
  "\\var" =          c("<var>", "</var>"),
  "\\verb" =         c("<pre>", "</pre>"),
  
  "RCODE" =          c("", ""),
  "VERB" =           c("", "")
)
