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
  } else if (tag == "\\eqn") {
    str_join("<code>", reconstruct(untag(rd[[1]])), "</code>")
  } else if (tag == "\\deqn") {
    if(length(rd) < 2)
      str_join("<code>", reconstruct(untag(rd[[1]])), "</code>")
    else
      str_join("<code>", reconstruct(untag(rd[[2]])), "</code>")
  } else if (tag == "\\url") {
    stopifnot(length(rd) == 1)
    str_join("<a href='", rd[[1]], "'>", rd[[1]], "</a>")

  } else if(tag == "\\email"){
    author_email(reconstruct(untag(rd)), rd[[1]][1])      
  } else if(tag == "COMMENT") {
    
    txt <- as.character(rd)
    str_replace(txt, "%", "#")
  } else if(tag == "\\enc"){
    
    reconstruct(rd[[1]])
    
  } else if(tag == "\\method" || tag == "\\S3method") {
    str_join(reconstruct(rd[[1]]),".",reconstruct(rd[[2]]))
  } else if(tag %in% c("\\dontshow", "\\testonly")){
    "" # don't show it to the user
  } else if(tag == "\\dontrun"){
    str_join("## <strong>Not run</strong>:", str_replace(reconstruct(untag(rd)), "\n", "\n#"), "## <strong>End(Not run)</strong>")

  } else if(tag == "\\special"){
#    "\\special" =      c("<em>","</em>"),    
    txt <- reconstruct(untag(rd))
    # replace '<' and '>' with html markings avoid browser misinterpretation
    txt <- str_replace(txt, "<", "&lt;")
    txt <- str_replace(txt, ">", "&gt;")
    txt <- str_replace(txt, "\\\\dots", "...")

    stupid <- unlist(str_match_all(txt, "\\\\[a-zA-Z]*"))
    for(i in seq_len(length(stupid)))
      message("Uknown tag (", stupid[i], ") found in 'special' tag")
    
    str_join("<em>", txt, "</em>")

  } else if(tag == "\\tabular"){
    parse_tabular(untag(rd))
  }else {
    
    message("Unknown tag ", tag)
    reconstruct(untag(rd))
  }
}

author_email <- function(name, address){
  str_join("<a href=\"mailto:",address,"?subject=(R-Help): \">",name,"</a>")
}

tag_simple <- function(tag, text) {
  stopifnot(length(text) == 1)
  html <- simple_tags[[tag]]
  str_join(html[1], text, html[2])
}

tag_link <- function(fun, pkg = NULL) {
  if (!is.null(pkg)) {
    str_join("<a href='/package/", pkg, "/topic/", fun, "'>", fun, "</a>")        
  } else {
    str_join("<a href='", function_help_path(fun), "'>", fun, "</a>")
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
#  "\\special" =      c("<em>","</em>"),
  "\\sQuote" =       c("&lsquo;","&rsquo;"),
  "\\strong" =       c("<strong>", "</strong>"),
#  "\\tab" =          c("&nbsp;&nbsp;", ""),
  "\\text" =         c("<p>", "</p>"),
  "\\var" =          c("<var>", "</var>"),
#  "\\verb" =         c("<pre>", "</pre>"),
  "\\verb" =         c("<code>", "</code>"),

  "RCODE" =          c("", ""),
  "VERB" =           c("", ""),
  "LIST" =          c("<ul>", "</ul>")
)

has_length <- function(x){
    !is.null(x) && NROW(x) > 0 && x != ""
}

dataframe_has_rows <- function(x){
  NROW(x) > 0
}

parse_text <- function(text){
  output <- suppressWarnings(parser(text = text))
  if(!dataframe_has_rows(attr(output, "data")))
    NULL
  else
    output
}

pluralize <- function(string, obj, plural = str_join(string, "s", sep = ""), bool_statement = has_length(obj)){
  if(bool_statement) {
    plural
  } else {
    string
  }
}

strip_html <- function(x) {
  str_replace(x, "</?.*?>", "")
}


#' Parse a Tabular Section
#' Parse a tabular section to include the text alignments
#'
#' @param tabular rd item to parsed
parse_tabular <- function(tabular){
  #' make all alignements into left, right or center
  alignments <- unlist(str_split(tabular[[1]][[1]], ""))
  alignments <- alignments[nchar(alignments) > 0]
  #' remove line markings
  alignments <- alignments[alignments != "|"]
  alignments <- c("r" = "right", "l" = "left", "c" = "center")[alignments]
  
  rows <- tabular[[2]]
  column <- 1
  output <- character(length(rows))
  
  #' Go through each item and reconstruct it if it is not a tab or carriage return
  for(i in seq_along(rows)){
    row_tag <- tag(rows[[i]])

    if(row_tag == "\\tab"){
      column <- column + 1
      output[i] <- str_join("</td><td align=\"", alignments[column], "\">")
    } else if(row_tag == "\\cr"){
      output[i] <- str_join("</td></tr><tr><td align=\"", alignments[1], "\">")
      column <- 1
    } else {
      output[i] <- reconstruct(rows[[i]])
    }
  }
  
  output[1] <- str_join("<table><tr><td align=\"", alignments[1], "\">", output[1])
  output[length(rows)] <- str_join(output[length(rows)], "</td></tr></table>")

  str_join(output, collapse = "")
}








