#' Check to see if solr is running.
#' A check to see if solr is running called and memoised at the start of \code{helpr()}
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
i_can_has_internetz <- memoise(function() {
  if (!allow_internetz()) return(FALSE)
  
  google <- url("http://google.com")
  on.exit(close(google))
  
  res <- try({suppresWarnings(open(google)); internet <- TRUE}, silent = TRUE)
  !inherits(res, "try-error")
})
