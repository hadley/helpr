#' Tag Something
#'
#' @param x item to be tagged
#' @keywords internal
#' @author Hadley Wickham
tag <- function(x) attr(x, "Rd_tag")

#' Untag Something
#'
#' @param x item to be untagged
#' @keywords internal
#' @author Hadley Wickham
untag <- function(x) {
  if (is.null(x)) return()
  attr(x, "Rd_tag") <- ""
  x
}


#' List tags
#' list all the tags within a object
#'
#' @param rd rd in question
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
list_tags <- function(rd){
  tags <- c()
  for(i in seq_along(rd)){
    tag_item <- tag(rd[[i]])
    if(length(tag_item) < 1)
      tag_item <- ""
    tags[i] <- tag_item
  }
  tags
}

#' Reconstruct R Documentation
#' recursively reconstruct R documentation
#'
#' @param rd rd in question
#' @author Hadley Wickham and Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
reconstruct <- function(rd) {
  
  if (is.null(rd)) return("")
  if (!is.list(rd) && is.null(attr(rd, "Rd_tag"))) return(as.character(rd))
  
  tag <- tag(rd)
    
  if(is.list(rd) & length(rd) > 0 & "\\item" %in% list_tags(rd)){
    reconstruct(parse_items(rd))
  }else if (length(tag) == 0 || tag == "TEXT" || tag == "") {
    # Collapse text strings
#    str_trim(str_join(sapply(rd, reconstruct), collapse = ""))
    
    as.character(str_join(sapply(rd, reconstruct), collapse = ""))

  } else if (is_section(tag)) {
    # Sections should be arranged into paragraphs    
    text <- reconstruct(untag(rd))
    paras <- str_trim(str_split(text, "\\n\\n")[[1]])
    str_join("<p>", paras, "</p>", "\n\n", collapse = "")      
    
  } else if (tag %in% names(simple_tags())) {
    # If we can process tag with just prefix & suffix, do so
    tag_simple(tag, reconstruct(untag(rd)))

  } else if (tag == "\\link") {
    stopifnot(length(rd) == 1)
    fun <- rd[[1]]
    pkg <- attr(rd, "Rd_option")
    
    if(!is.null(pkg)){
      if(str_sub(pkg, end = 1) == "=")
        tag_link(str_sub(pkg, start = 2))
      else
        tag_link(fun, pkg)
    }else{
      tag_link(fun, pkg)
    }
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
    txt <- str_replace(txt, "<", "&#60;")
    txt <- str_replace(txt, ">", "&#62;")
    txt <- str_replace(txt, "\\\\dots", "...")

    stupid <- unlist(str_match_all(txt, "\\\\[a-zA-Z]*"))
    for(i in seq_len(length(stupid)))
      message("Uknown tag (", stupid[i], ") found in 'special' tag")
    
    str_join("<em>", txt, "</em>")

  } else if(tag == "\\tabular"){
    parse_tabular(untag(rd))
  } else if(tag %in% c("\\ifelse", "\\if")){
    if("html" == rd[[1]][[1]]){
      reconstruct(rd[[2]])
    }else if(tag == "\\ifelse"){
      reconstruct(rd[[3]])      
    }
    
  } else if(tag == "\\S4method"){
    str_join("## S4 method for signature \"",reconstruct(rd[[2]]),"\":\n", reconstruct(rd[[1]]), sep ="")

  } else if(tag == "\\linkS4class"){
    item <- reconstruct(untag(rd))

    pkg <- tryCatch(
        attr(getClass(item)@className, "package"),
        error = function(e){
          message("can't find the package for s4class: ", item)
          "no_package"
        }
      )
    if(pkg == "no_package"){
      item
    }else{
      pkg <- attr(getClass(item)@className, "package")
      tag_link(item, pkg, str_join(item, "-class", collapse = ""))
    }
  } else if(tag == "\\Sexpr"){
    expr <- eval(parse(text = rd), envir=globalenv())
    con <- textConnection(expr)
    value <- reconstruct(parse_Rd(con,fragment=T))
    close(con)
    value
    
  }else {
    message("Unknown tag ", tag, ". Please contact the 'helpr' maintainer. Thank you.")
    reconstruct(untag(rd))
  }
}

#' Author Email Parsing
#' parse the author email
#'
#' @param name name of the email
#' @param address email address
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
author_email <- function(name, address){
  str_join("<a href=\"mailto:",address,"?subject=(R-Help): \">",name,"</a>")
}

#' tag a simple item
#' tag an item that is contained in the simple tags list
#'
#' @param tag tag looking for
#' @param text text to go in the tag
#' @author Hadley Wickham
#' @keywords internal
tag_simple <- function(tag, text) {
  stopifnot(length(text) == 1)
  html <- simple_tags()[[tag]]
  str_join(html[1], text, html[2])
}

#' link to another topic
#'
#' @param fun function to tag
#' @param pkg package to look in
#' @param topic_page topic page of the function
#' @author Hadley Wickham and Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
tag_link <- function(fun, pkg = NULL, topic_page = fun) {
  if (!is.null(pkg)) {
    str_join("<a href=\"/package/", pkg, "/topic/", topic_page, "\">", fun, "</a>")        
  } else {
    str_join("<a href=\"", function_help_path(fun), "\">", fun, "</a>")
  }
}


#' Is Section
#' determines whether or not it is a section
#'
#' @param tag check to see if the tag is in a list of other tags
#' @author Hadley Wickham
#' @keywords internal
is_section <- function(tag) {
  tag %in% c("\\details", "\\description", "\\value", "\\author", "\\seealso")
}

#' simple tags
#' list all the tags that can be parsed in a simple way
#'
#' @author Hadley Wickham
#' @keywords internal
simple_tags <- function(){ 
list(
  "\\acronym" =      c('<acronym>','</acronym>'),
  "\\bold" =         c("<b>", "</b>"),
  "\\cite" =         c("<cite>", "</cite>"),
  "\\code" =         c("<code>", "</code>"),
  "\\command" =      c("<code>", "</code>"),
  "\\cr" =           c("<br >", ""),
  "\\describe" =     c("<span class=\"describe\">", "</span>"),
  "\\dfn" =          c("<dfn>", "</dfn>"),
  "\\donttest" =     c("", ""),
  "\\dots" =         c("...", ""),
  "\\dquote" =       c("&#147;", "&#148;"),
  "\\dQuote" =       c("&#147;", "&#148;"),
  "\\emph" =         c("<em>", "</em>"),
  "\\enumerate" =    c("<ol>", "</ol>"),
  "\\env" =          c('<span class = "env">', '</span>'),
  "\\file" =         c('&#145;<span class = "file">', '</span>&#146;'),
  "\\item" =         c("<li>", "</li>"),
  "\\itemize" =      c("<ul>", "</ul>"),
  "\\kbd" =          c("<kbd>", "</kbd>"),
  "\\ldots" =        c("...", ""),
  "\\option" =       c('<span class = "option">',"</span>"),
  "\\out" =          c("", ""),
  "\\pkg" =          c('<span class = "pkg">',"</span>"),
  "\\preformatted" = c("<pre>","</pre>"),
  "\\R" =            c('<span style="R">R</span>', ""),
  "\\samp" =         c('<span class = "samp">',"</span>"),
  "\\sQuote" =       c("&#145;","&#146;"),
  "\\strong" =       c("<strong>", "</strong>"),
  "\\text" =         c("<p>", "</p>"),
  "\\var" =          c("<var>", "</var>"),
  "\\verb" =         c("<code>", "</code>"),

  "RCODE" =          c("", ""),
  "VERB" =           c("", ""),
  "LIST" =          c("<ul>", "</ul>")
)}

#' Has Text
#' determines whether or not the item has text
#'
#' @param x item in question
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
has_text <- function(x){
  trim <- str_trim(x)
  !is.null(x) && trim != "" && length(trim) > 0
}

#' Dataframe has Rows
#' determines whether or not the dataframe has rows
#'
#' @param x item in question
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
dataframe_has_rows <- function(x){
  NROW(x) > 0
}

#' Parse Text
#' wrapper to parser's \code{parser(text = text)}
#'
#' @param text text to be parsed
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
parse_text <- function(text){
  output <- suppressWarnings(parser(text = text))
  if(!dataframe_has_rows(attr(output, "data")))
    NULL
  else
    output
}

#' Pluralize
#' pluralize a string with either the default 's' according to the boolean statement
#'
#' @param string string to be pluralized
#' @param obj object to look at
#' @param plural plural string
#' @param bool_statement boolean to use to determine which string to use
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal 
pluralize <- function(string, obj, plural = str_join(string, "s", sep = ""), bool_statement = NROW(obj)){
  if(bool_statement) {
    plural
  } else {
    string
  }
}

#' Strip HTML
#' strip the HTML from a text string
#'
#' @param x text string in question
#' @author Hadley Wickham
#' @keywords internal 
strip_html <- function(x) {
  str_replace(x, "</?.*?>", "")
}


#' Parse a Tabular Section
#' Parse a tabular section to include the text alignments
#'
#' @param tabular rd item to parsed
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal 
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


#' Parse List with Items
#' parse a list containing "\\item" tags.
#'
#' It will replace the items with plan, non-attributed text.  It needs to be a 'pre-parser' as it must be done before the whole list is reconstructed
#' @param rd R documentation item to be altered and then returned
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal 
parse_items <- function(rd){
  tags <- list_tags(rd)
  is_items <- rep(TRUE, length(tags))

  for(i in rev(seq_along(tags))){
    if(tags[i] != "\\item"){
      rd_i <- reconstruct(rd[[i]])
      if(str_trim(rd_i) != ""){
        is_items[i] <- FALSE
      }else if(str_trim(rd_i) == ""){
        if(i == length(tags)){
          is_items[i] <- FALSE
        }else{
          is_items[i] <- is_items[i+1]
        }
      }
        
    }
  }
    
  item_groups <- group_int_arr(subset(seq_along(tags), is_items))
#  non_item_groups <- group_int_arr(subset(seq_along(tags), !is_items))
 
  for(i in item_groups){
    item_text <- parse_item_list(rd[i])
    rd[i] <- ""
    rd[i[1]] <- item_text
  }
  
  rd
}


#' Group Integer Array
#' Group items into similar sections
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal 
group_int_arr <- function(arr){
  n <- length(arr)
  groups <- c(0, cumsum(arr[-n] != arr[-1] - 1))
  split(arr, groups)
}

#' Parse Item List
#' parse a group of "\\item" into a table with a bold item and reconstructed description
#'
#' @param rd item to be parsed
#' return table text with no attributes
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal 
parse_item_list <- function(rd){
  tags <- list_tags(rd)
  items <- rd[tags == "\\item"]
  
  items_text <- sapply(items, function(x){
    if(length(x) < 1){
      # small item in item list
      ""
    }else{
      str_join("<tr><td><strong>",reconstruct(x[[1]]), "</strong></td><td>", reconstruct(x[[2]]), "</td></tr>", collapse = "")
    }
  })
  
  str_join("<table>", str_join(items_text, collapse = ""), "</table>", collapse = "")
}



