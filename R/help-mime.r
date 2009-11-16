mime_type <- function(path) {
  ext <- strsplit(path, ".", fixed = TRUE)[[1L]]
  n <- length(ext)
  
  if (n == 0) return()
  
  c(
    "css" = "text/css",
    "gif" = "image/gif", # in R2HTML
    "js" = "text/javascript",
    "jpg" = "image/jpeg",
    "html" = "text/html",
    "pdf" = "application/pdf",
    "eps" = "application/postscript",
    "ps" = "application/postscript", # in GLMMGibbs, mclust
    "sgml"= "text/sgml", # in RGtk2
    "xml" = "text/xml",  # in RCurl
    "text/plain"
  )[ext[n]]
}