#http://lucene.apache.org/solr/tutorial.html
#http://localhost:8983/solr/select/?q=Samsung&version=2.2&start=0&rows=10&indent=on
#load_html("/search/q=YaleToolkit;start=0")

#' make a field for a solr document
make_field <- function(name, value){
  value <- str_trim(value)
  value <- str_replace(value, "\n", "")
  value <- str_replace(value, "\t", "")
  if(!identical(name, "id")) name <- str_join(name, "_t")
  str_join("<field name=\"", name, "\">", str_join(value, collapse = "; "),"</field>", collapse = "")
}


#' turn a list into a solr doc
list_to_xml <- function(id, obj){
  
  obj$id <- id
  
  new_obj <- list_to_double_list(obj)

  fields <- sapply(new_obj, function(x){
    make_field(x$name, x$value)
  })
  
  str_join("<doc>", str_join(fields, collapse=""), "</doc>", collapse = "")
  
}


#' make a list into a nested list
#' this is to be done to easily use sapply and keep the name of the item
list_to_double_list <- function(obj){
  new_obj <- list()
  for(item_name in names(obj)){
    new_obj[[item_name]] <- list(name = item_name, value = obj[[item_name]])
  }
  new_obj
}


#' make it so the xml is an 'add'
make_add_xml <- function(obj){
  str_join("<add>", obj, "</add>", collaspe = "")
}


#' Save page info into xml for solr
#'   
#' @examples
#'   save_xml("all_topics.xml", make_add_xml(helpr_topic_xml_all("y")))
save_xml <- function(file_name, txt){
  txt <- str_replace(txt, "<doc>", "<doc>\n\t")
  txt <- str_replace(txt, "</field>", "</field>\n\t")
  txt <- str_replace(txt, "\t</doc>", "</doc>")
  cat(txt, file = file_name)
}


#' helpr topic into xml
#'
#' @param package package to use
#' @param topic topic to explore
helpr_topic_xml <- function(package, topic){
  topic_html_args <- helpr_topic(package, topic)
  
  
  # strip the html of the list objects
  # do not bother: name, keywords, title, package
  topic_html_args$aliases <- strip_html(topic_html_args$aliases)
  topic_html_args$desc <- strip_html(topic_html_args$desc)
  topic_html_args$details <- strip_html(topic_html_args$details)
  topic_html_args$value <- strip_html(topic_html_args$value)
  topic_html_args$examples <- NULL
  topic_html_args$example_functions <- NULL
  topic_html_args$example_functions_str <- NULL
  topic_html_args$usage <- NULL
  topic_html_args$authors <- strip_html(topic_html_args$authors)
  topic_html_args$author_str <- NULL
  topic_html_args$seealso <- NULL
  topic_html_args$params <- NULL
  
  
  list_to_xml(
    str_join("/package/", package, "/topic/", topic, collapse = ""),
    topic_html_args
  )
}

helpr_topic_xml_all <- function(start_letter = "a", verbose = TRUE){
  packages <- suppressWarnings(library()$results[,"Package"])
  packages <- packages[order(tolower(packages))]
  first_letter <- sapply(strsplit(packages, ""), function(x){tolower(x[1])})
  rows <- str_detect(first_letter, str_join("[", tolower(start_letter), "-z]"))
  packages <- packages[rows]
  
  output <- c()
  
  for(j in seq_along(packages)){
    if(verbose)
      cat("\n\n\n", packages[j],"\n")
    all_topics <- pkg_topics_index(packages[j])
    unique_topics <- all_topics[!duplicated(all_topics$file), "alias"]

    pkg_output <- c()
    for (i in seq_along(unique_topics)) {
      if(verbose)
        cat(i,": ", unique_topics[i],"... ")
      start_time <- Sys.time()
      pkg_output[i] <- helpr_topic_xml(packages[j], unique_topics[i])
      time <- Sys.time() - start_time
      if(verbose)
        cat("  ", str_sub(capture.output(time), 20), "\n")
       
    }
    output[j] <- str_join(
      "\n\n\n<!--         ", packages[j], "         -->\n", 
      str_join(pkg_output, collapse = "\n\n")
      , collapse = "")

  }
  str_join(output, collapse = "")
}


helpr_search_row_count <- 10

get_solr_query <- function(query_string){
  rows <- helpr_search_row_count
  xml_response <- xmlTreeParse(str_join("http://0.0.0.0:8983/solr/select/?version=2.2&rows=",rows,"&indent=on&", query_string), isURL = TRUE)$doc$children
  
  docs <- xml_response$response[2]$result
  header <- xml_response$response[1]
  query <- as.character(header$lst[[3]][[3]][[1]]$value)

  start_pos <- as.numeric(docs$attributes["start"])

  total_item_count <- as.numeric(docs$attributes["numFound"])
  list(
    response = docs, 
    items_before = start_pos, 
    items_after = max(0, total_item_count - start_pos - rows),
    total_item_count = total_item_count, 
    query = query
  )
}

xmlResponse_to_list <- function(xml_response){
  return_item <- list()
  for(i in seq_along(xml_response))
    return_item[[i]] <- xmlDoc_to_list(xml_response[[i]])
  return_item
}

xmlDoc_to_list <- function(xml_response){
  nodes <- xml_response
  
  new_list <- list()
  
  for(i in seq_along(nodes)){
    node <- nodes[[i]]
    attr(node, "class") <- NULL
    field <- str_replace(node$attributes, "_t", "")
    if(identical(field, "id")) field <- "url"
    
    value <- node[3]$children$text$value
    if(is.null(value)) value <- ""
    
    new_list[[field]] <- value
  }
  
  new_list
}

package_and_topic_from_url <- function(url_txt){
  pkg <- ""
  topic <- ""
  if(str_detect(url_txt, "/package/")){
    pkg <- str_extract(url_txt, "/package/[a-zA-Z_0-9]*/")
    pkg <- str_replace(pkg, "/package/", "")
    pkg <- str_replace(pkg, "/", "")
    
    if(str_detect(url_txt, "/topic/")){
      topic <- str_extract(url_txt, "/topic/[a-zA-Z_0-9]*")
      topic <- str_replace(topic, "/topic/", "")
      topic <- str_replace(topic, "/", "")      
    }
  }
  
  list(pkg = pkg, topic = topic)
}

package_description <- function(pkg, topic){
  gsub("$\n+|\n+^", "", reconstruct(pkg_topic(pkg, topic)$description))
}

search_query_path <- function(query, start_pos){
  str_join(base_html_path(),"/search/start=",start_pos,";q=",query)  
}


helpr_solr_search <- function(query_string){
  xml <- get_solr_query(query_string)
  items <- xmlResponse_to_list(xml$response)
  
  urls <- sapply(items, function(x) x$id)
  desc <- sapply(items, function(x) x$desc)
  
  list(
    urls = urls,
    desc = desc,
    items_before = xml$items_before,
    items_after = xml$items_after,
    query = xml$query,
    start_pos = xml$items_before,
    row_count = helpr_search_row_count,
    total_item_count = xml$total_item_count
  )
  
  
}




