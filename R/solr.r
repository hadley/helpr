#' Solr base URL.
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
solr_base_url <- function() {
  # "http://localhost:8983"
  "http://helpr32.mmx-dns.com:8080"
}



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


# # Produce a message that states Solr is not running.
# #
# # @author Barret Schloerke \email{schloerke@@gmail.com}
# # @keywords internal
# solr_FAIL <- function() {
#   message("Full text search is not available, yet.  Please wait for future versions.")
#   NULL
# }








#' Pkg and topic from URL.
#' Retrieve the pkg and topic from the URL
#'
#' @param url_txt url to be parsed
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
package_and_topic_from_url <- function(url_txt) {
  pkg <- ""
  topic <- ""
  if (str_detect(url_txt, "/package/")) {
    pkg <- str_extract(url_txt, "/package/[a-zA-Z_.0-9]*/")
    pkg <- str_replace_all(pkg, "/package/", "")
    pkg <- str_replace_all(pkg, "/", "")
    
    if (str_detect(url_txt, "/topic/")) {
      topic <- str_split(url_txt, "/topic/")[[1]][2]
    }
  }
  
  list(pkg = pkg, topic = topic)
}




#' Helpr Search.
#'
#' @param query_list list that contains the start position, query, and other parameters
#' @return returns all the necessary information from a search
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
helpr_solr_search <- function(query_list) {
  if (! i_can_has_internetz()) return(NULL)

  result <- get_solr_query_result(query_list)
  items <- result$response
  
  urls <- as.character(names(items))
  
  list(
    urls = urls,
    items = items,
    items_before = result$items_before,
    items_after = result$items_after,
    query = result$query,
    start_pos = result$items_before,
    row_count = 20,
    total_item_count = result$total_item_count
  )
  
  
}



