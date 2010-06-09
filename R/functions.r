#' Work out the source code of a function.
body_text <- function(fun) {
  str_c(deparse(body(fun)), collapse = "\n")
}

#' Count how many times a function calls other functions.
# 
#' @param fun string giving name of function, or function
#' @export
function_calls <- function(fun) {
  text <- body_text(fun)
  
  pieces <- attr(parser(text = text), "data")
  calls <- subset(pieces, token.desc == "SYMBOL_FUNCTION_CALL")$text
  
  as.data.frame(table(fun = calls), responseName = "freq",
    stringsAsFactors = FALSE)
}
