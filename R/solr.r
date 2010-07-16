#http://lucene.apache.org/solr/tutorial.html
#http://localhost:8983/solr/select/?q=Samsung&version=2.2&start=0&rows=10&indent=on

#' make a field for a solr document
make_field <- function(name, value){
  value <- str_trim(value)
  value <- str_replace(value, "\n", "")
  value <- str_replace(value, "\t", "")
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
#'   save_xml("example.xml", helpr_topic_xml("grDevices", "png"))
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
      "\n\n\n<!----------", packages[j], "---------->\n", 
      str_join(pkg_output, collapse = "\n\n")
      , collapse = "")

  }
  str_join(output, collapse = "")
}


