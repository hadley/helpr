#' Topic fields.
#'
#' @author Barret Schloerke
#' @keywords internal
solr_topic_fields <- function(){
  c("Title_t", "Description_t", "Details_t", "Value_t", "Authors_t")
}


#' Topic query. 
#'
#' @author Barret Schloerke
#' @keywords internal
solr_query_topic_fields <- function(query, collapse = " OR ") {
  str_c(solr_topic_fields(), ":", query, collapse = collapse)
}


#' Example query field.
#'
#' @author Barret Schloerke
#' @keywords internal
solr_example_field <- function() {
  "Examples_t"
}


#' Example query.
#'
#' @author Barret Schloerke
#' @keywords internal
solr_query_example_field <- function(query, collapse = " OR ") {
  str_c(solr_example_field(),":", query, collapse = collapse)    
}


#' Combine Solr parameters.
#'
#' @author Barret Schloerke
#' @keywords internal
solr_combine_param <- function(value, param = names(value)) {
  str_c(param, value, sep = "=", collapse = "&")
}


#' Solr Topics in Example
#'
#' @author Barret Schloerke
#' @keywords internal
solr_has_topic_in_example <- function(topics) {
  if (! i_can_has_internetz()) return(data.frame())
  
  query_list <- list(query = topics)
  
  result <- get_solr_query_result(query_list, TRUE)
  if(result$total_item_count == 0)
    return(data.frame())

  t(sapply(result$response, function(x) { 
    path <- package_and_topic_from_url(x$id)
    c(title = x$Title_t, pkg = path$pkg, topic = path$topic) 
  }))
}


#' Search query path.
#' return a html path for a search
#'
#' @param query query to be used
#' @param start_pos postion to start at
#' @author Barret Schloerke
#' @keywords internal
search_query_path <- function(query="example", start_pos=0) {
  str_c(base_html_path(), "/search/start=", start_pos, ";q=", query)  
}



#' Similar pages.
#' Find related pages and return info in a data.frame
#'
#' @param topic title to be used to find similar results
#' @author Barret Schloerke
#' @keywords internal
solr_similar <- function(topic) {
  if (! i_can_has_internetz()) return(data.frame())
  
  site <- str_c(solr_base_url(), "/solr/select?wt=json&mlt=true&mlt.count=5&mlt.fl=Title_t,Description_t&q=", solr_query_topic_fields(topic))
  output <- suppressWarnings(urlJSON_to_list(site))
  
  if (length(output$moreLIkeThis) < 1) return(data.frame())
  
  docs <- output$moreLikeThis[[1]]$docs
  
  t(sapply(docs, function(x) { 
    path <- package_and_topic_from_url(x$id)
    c(title = x$Title_t, pkg = path$pkg, topic = path$topic) 
  }))
  
}


#' Solr query.
#' Retrieve a solr query 
#'
#' @param query_list list that contains the start position, query, and other parameters that are ready to be pasted
#' @author Barret Schloerke
#' @keywords internal
get_solr_query_result <- function(query_list, function_field = FALSE) {
  
  if(is.null(query_list$query))
    stop("query was not included in the query_list")
    
  other <- query_list$other
  if(is.null(other))
    other <- ""
  else
    other <- str_c("&", other)

  start_pos <- query_list$start
  if(is.null(start_pos))
    start_pos <- 0
  
  rows <- 20
  response <- urlJSON_to_list(str_c(solr_base_url(), "/solr/select/?",
    "version=2.2",
    "&wt=json",
    "&rows=", rows, 
    "&start=", start_pos,
    "&indent=on",
    if (identical(function_field, FALSE)) {
      str_c("&hl=on",
        "&hl.simple.pre=<strong>",
        "&hl.simple.post=</strong>",
        "&hl.fragsize=70",
        "&hl.mergeContiguous=true",
        "&hl.fl=", str_c(solr_topic_fields(), collapse = ","),
        "&q=", solr_query_topic_fields(query_list$query))
    } else {
      str_c(
        "&fl=", solr_example_field(), ",id,Title_t",
        "&q=", solr_query_example_field(query_list$query)
      )
    }
    ,other
  ))

  if (identical(function_field, FALSE)) {
    docs <- response$highlighting
  } else {
    docs <- response$response$docs
  }
  
  query <- query_list$query

  start_pos <- as.numeric(response$responseHeader$params$start)
  if (is.na(start_pos) || is.null(start_pos)) {
    start_pos <- 0
  }
  total_item_count <- as.numeric(response$response$numFound)

  list(
    response = docs, 
    items_before = start_pos, 
    items_after = max(0, total_item_count - start_pos - rows),
    total_item_count = total_item_count, 
    query = query
  )
}




