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
    
    if (length(tag) >0 && tag == "link") {
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

html_tags <- list(
  "acronym" = c('<acronym><span class = "acronym">','</span></acronym>'),
  "bold" =         c("<b>", "</b>"),
  "cite" =         c("<cite>", "</cite>"),
  "code" =         c("<code>", "</code>"),
  "command" =      c("<code>", "</code>"),
  "dfn" =          c("<dfn>", "</dfn>"),
  "donttest" =     c("", ""),
  "dquote" =       c("&ldquo;", "&rdquo;"),
  "emph" =         c("<em>", "</em>"),
  "enumerate" =    c("<ol>", "</ol>"),
  "env" =          c('<span class = "env">', '</span>'),
  "file" =         c('&lsquo;<span class = "file">', '</span>&rsquo;'),
  "link" =         c("<a>", "</a>"),
  "item" =         c("<li>", ""),
  "itemize" =      c("<ul>", "</ul>"),
  "kbd" =          c("<kbd>", "</kbd>"),
  "option" =       c('<span class = "option">',"</span>"),
  "pkg" =          c('<span class = "pkg">',"</span>"),
  "preformatted" = c("<pre>","</pre>"),
  "samp" =         c('<span class = "samp">',"</span>"),
  "squote" =       c("&lsquo;","&rsquo;"),
  "strong" =       c("<strong>", "</strong>"),
  "var" =          c("<var>", "</var>"),
  "verb" =         c("<pre>", "</pre>"),
  "text" =         c("<p>", "</p>")
)


escapes <- list(
 "R" = '<span style="R">R</span>',
 "cr" = "<br >",
 "dots" = "...",
 "ldots" = "..."
)