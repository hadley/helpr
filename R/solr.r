
#' helpr topic into xml for solr
#'
#' @param package package to use
#' @param topic topic to explore
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
solr_topic <- function(package, topic){
  
  rd <- pkg_topic(package, topic)
  tags <- sapply(rd, tag)

  # Remove top-level text strings - just line breaks between sections
  rd <- rd[tags != "TEXT"]

  out <- list()
  
  # Join together aliases and keywords
  out$Aliases <- setdiff(
    unname(sapply(rd[names(rd) == "alias"], "[[", 1)),
    strip_html(reconstruct(untag(rd$name)))
  )
  out$Keywords <- unname(sapply(rd[names(rd) == "keyword"], "[[", 1))

  # Title, description, value and examples, need to be stitched into a 
  # single string.
  out$Title <- strip_html(reconstruct(untag(rd$title)))
  out$Description <- gsub("$\n+|\n+^", "", strip_html(reconstruct(rd$description)))
  out$Details <- strip_html(reconstruct(rd$details))
  out$Value <- strip_html(reconstruct(rd$value))
  out$Authors <- strip_html(reconstruct(rd$author))
  out$Package <- str_c(package, " (", pkg_version(package), ")", collapse = "")

  list_to_xml(
    str_c("/package/", package, "/topic/", topic, collapse = ""),
    out
  )
}


#' Read URL
#' Retrieve the text from a url
#' 
#' @param url_string url to explore
#' @return plain text from that url
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
read_url <- function(url_string){
  url_connect <- url(utils::URLencode(url_string))
  on.exit(close(url_connect))
  output <- suppressWarnings(str_c(readLines(url_connect), collapse = ""))
  output
}

#' URL made of JSON to list
#'
#' @param url_string url that contains a JSON output to be turned into a list
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
urlJSON_to_list <- function(url_string){
  rjson::fromJSON(read_url(url_string))
}

#' make a xml field
#' make a field for a solr document
#' 
#' @param name name of the field
#' @param value value of the field
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
make_field <- function(name, value){
  if(length(value) < 1)
    value <- ""
  value <- str_trim(value)
  value <- gsub("&(?![#]{1})", "&#38;", value, perl=TRUE)
  value <- str_replace(value, "<", "&#60;")
  value <- str_replace(value, ">", "&#62;")
  value <- str_replace(value, "\n", "")
  value <- str_replace(value, "\t", "")
  if(!identical(name, "id")) name <- str_c(name, "_t")
  str_c("<field name=\"", name, "\">", str_c(value, collapse = "; "),"</field>", collapse = "")
}


#' turn a list into a solr doc
#' turn a list into a solr doc
#' 
#' @param id id tag to be used
#' @param obj list to perform on
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
list_to_xml <- function(id, obj){
  
  obj$id <- id
  new_obj <- list_to_double_list(obj)

  fields <- sapply(new_obj, function(x){
    make_field(x$name, x$value)
  })
  
  str_c("<doc>", str_c(fields, collapse=""), "</doc>", collapse = "")
}


#' make a list into a nested list
#' this is to be done to easily use sapply and keep the name of the item
#'
#' @param obj list to perform on
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
list_to_double_list <- function(obj){
  new_obj <- list()
  for(item_name in names(obj)){
    new_obj[[item_name]] <- list(name = item_name, value = obj[[item_name]])
  }
  new_obj
}


#' make it so the xml is an 'add'
#' make it so the xml is an 'add' to be commited to solr
#'
#' @param obj list to perform on
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
make_add_xml <- function(obj){
  str_c("<add>", obj, "</add>", collaspe = "")
}


#' Save page info
#' Save page info into xml for solr
#'
#' @param txt xml text string
#' @param file_name location to save the file. Defaults to a temp file that is discarded when R shuts down.
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
save_xml <- function(txt, file_name=tempfile()){
  txt <- str_replace(txt, "<doc>", "<doc>\n\t")
  txt <- str_replace(txt, "</field>", "</field>\n\t")
  txt <- str_replace(txt, "\t</doc>", "</doc>")
  cat(txt, file = file_name)
  file_name
}

#' Index topic
#' Index a topic into solr
#'
#' @export
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @param package package in question
#' @param topic topic in question
index_topic <- function(package, topic){
  put_string(make_add_xml(solr_topic(package, topic)))
}

#' Index package
#' Index a whole package into solr
#'
#' @export
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @param package package in question
#' @param start_letter used when you want to start froma certain letter, such as 'q'
#' @param verbose should output be shown?
index_package <- function(package, start_letter = "a", verbose = TRUE){
  if(verbose == TRUE)
    cat("\n\n\n")
  if(verbose == TRUE || verbose == "package")
    cat(package,"\n")
  require(package, character.only=TRUE)
  all_topics <- pkg_topics_index(package)
  unique_topics <- all_topics[!duplicated(all_topics$file), "alias"]
  if(length(unique_topics) > 0){
    first_letter <- sapply(strsplit(unique_topics, ""), function(x){tolower(x[1])})
    rows <- str_detect(first_letter, str_c("[", tolower(start_letter), "-z]"))
    unique_topics <- unique_topics[rows]
  }

  pkg_output <- c()
  for (i in seq_along(unique_topics)) {
    if(verbose==TRUE)
      cat(i,": ", unique_topics[i],"... ")
    start_time <- Sys.time()
    pkg_output[i] <- solr_topic(package, unique_topics[i])
    time <- Sys.time() - start_time
    if(verbose==TRUE)
      cat("  ", str_sub(capture.output(time), 20), "\n")
      
  }
  put_string(make_add_xml(
    str_c(
      "\n\n\n<!--         ", package, "         -->\n", 
      str_c(pkg_output, collapse = "\n\n")
      , collapse = "")
  ))

}

#' Index all packages
#' Index all packages into solr
#'
#' @export
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @param start_letter used when you want to start froma certain letter, such as 'q'
#' @param verbose should output be shown?
index_all <- function(start_letter = "a", verbose = TRUE){
  packages <- installed_packages()$Package
  packages <- packages[order(tolower(packages))]
  first_letter <- sapply(strsplit(packages, ""), function(x){tolower(x[1])})
  rows <- str_detect(first_letter, str_c("[", tolower(start_letter), "-z]"))
  packages <- packages[rows]
  
  sapply(packages, index_package, verbose = verbose)
  if(verbose)
    "Finished"
  else
    invisible("Finished")
}


#' Solr Query
#' Retrieve a solr query 
#'
#' @param solr_param_string parameters that are to be passed onto solr
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
get_solr_query_result <- function(solr_param_string){
  rows <- 20
  response <- urlJSON_to_list(str_c("http://0.0.0.0:8983/solr/select/?version=2.2&wt=json&rows=", rows, "&indent=on&hl=on&hl.simple.pre=<strong>&hl.simple.post=</strong>&hl.fl=*&hl.fragsize=70&hl.mergeContiguous=true&", solr_param_string))
  docs <- response$highlighting
  query <- response$responseHeader$params$q

  start_pos <- as.numeric(response$responseHeader$params$start)

  total_item_count <- as.numeric(response$response$numFound)
  list(
    response = docs, 
    items_before = start_pos, 
    items_after = max(0, total_item_count - start_pos - rows),
    total_item_count = total_item_count, 
    query = query
  )
}


#' Pkg and topic from url
#' Retrieve the pkg and topic from the url
#'
#' @param solr_param_string parameters that are to be passed onto solr
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
package_and_topic_from_url <- function(url_txt){
  pkg <- ""
  topic <- ""
  if(str_detect(url_txt, "/package/")){
    pkg <- str_extract(url_txt, "/package/[a-zA-Z_0-9]*/")
    pkg <- str_replace(pkg, "/package/", "")
    pkg <- str_replace(pkg, "/", "")
    
    if(str_detect(url_txt, "/topic/")){
      topic <- str_split(url_txt, "/topic/")[[1]][2]
    }
  }
  
  list(pkg = pkg, topic = topic)
}

#' Package description
#'
#' @param pkg package in question
#' @param topic topic in question
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
package_description <- function(pkg, topic){
  gsub("$\n+|\n+^", "", reconstruct(pkg_topic(pkg, topic)$description))
}

#' search query path
#' return a html path for a search
#'
#' @param query query to be used
#' @param start_pos postion to start at
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
search_query_path <- function(query, start_pos){
  str_c(base_html_path(),"/search/start=",start_pos,";q=",query)  
}


#' Helpr Search
#'
#' @return returns all the necessary information from a search
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
helpr_solr_search <- function(query_string){
  result <- get_solr_query_result(query_string)
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


#' Send commit command to Solr
#' Send a commit command to Solr to finalized any submissions
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
send_commit_command <- function(){
  send_system_command("curl http://localhost:8983/solr/update --data-binary '<commit/>' -H 'Content-type:text/xml; charset=utf-8'")
}

#' Send system command to Solr
#' Send a system command to Solr to add / update files to Solr
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
send_system_command <- function(system_string){
  curled_text <- system(system_string, intern = TRUE, ignore.stderr = TRUE)
  status <- str_sub(curled_text[3], start=47, end=47)
  if(length(status) < 1)
    status <- "FAIL"
  
  if(status != "0"){
    message(str_c(curled_text, collapse = "\n"))
    stop("Error uploading file to solr")
  }
  
}

#' PUT a string to the Solr server
#' PUT a string to the Solr server
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
put_string <- function(string){
  file_name <- save_xml(string)
  put_file(file_name)
}

#' PUT a file to the Solr server
#' PUT a file to the Solr server
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
put_file <- function(file_name){
  cat("posting file: ", file_name,"\n")
  send_system_command(str_c("curl http://localhost:8983/solr/update --data-binary @", file_name, " -H 'Content-type:text/xml; charset=utf-8'", collapse = ""))
  send_commit_command()
}

