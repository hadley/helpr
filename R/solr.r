#http://lucene.apache.org/solr/tutorial.html
#http://localhost:8983/solr/select/?q=Samsung&version=2.2&start=0&rows=10&indent=on
#load_html("/search/q=YaleToolkit;start=0")


#solr_topic, index_topic, index_package, index_all
#post_file
#index_top <- function(x) post_topic(solr_topic(x)))
#solr_topic should be a rewrite of reconstruct


#' helpr topic into xml for solr
#'
#' @param package package to use
#' @param topic topic to explore
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
  out$Package <- str_join(package, " (", pkg_version(package), ")", collapse = "")

  list_to_xml(
    str_join("/package/", package, "/topic/", topic, collapse = ""),
    out
  )
}


#' Read URL
#' Retrieve the text from a url
#' 
#' @param url_string url to explore
#' @return plain text from that url
read_url <- function(url_string){
  url_connect <- url(utils::URLencode(url_string))
  output <- suppressWarnings(str_join(readLines(url_connect), collapse = ""))
  close(url_connect)
  output
}

#' URL made of JSON to list
#'
#' @param url_string url that contains a JSON output to be turned into a list
urlJSON_to_list <- function(url_string){
  rjson::fromJSON(read_url(url_string))
}

#' make a field for a solr document
make_field <- function(name, value){
  if(length(value) < 1)
    value <- ""
  value <- str_trim(value)
  value <- gsub("&(?![#]{1})", "&#38;", value, perl=TRUE)
  value <- str_replace(value, "<", "&#60;")
  value <- str_replace(value, ">", "&#62;")
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
#'   save_xml(make_add_xml(index_all("y")), "all_topics.xml")
save_xml <- function(txt, file_name=tempfile()){
  txt <- str_replace(txt, "<doc>", "<doc>\n\t")
  txt <- str_replace(txt, "</field>", "</field>\n\t")
  txt <- str_replace(txt, "\t</doc>", "</doc>")
  cat(txt, file = file_name)
  file_name
}

index_topic <- function(package, topic){
  put_string(make_add_xml(solr_topic(package, topic)))
}

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
    rows <- str_detect(first_letter, str_join("[", tolower(start_letter), "-z]"))
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
    str_join(
      "\n\n\n<!--         ", package, "         -->\n", 
      str_join(pkg_output, collapse = "\n\n")
      , collapse = "")
  ))

}


index_all <- function(start_letter = "a", verbose = TRUE){
  packages <- suppressWarnings(library()$results[,"Package"])
  packages <- packages[order(tolower(packages))]
  first_letter <- sapply(strsplit(packages, ""), function(x){tolower(x[1])})
  rows <- str_detect(first_letter, str_join("[", tolower(start_letter), "-z]"))
  packages <- packages[rows]
  
  sapply(packages, index_package, verbose = verbose)
  if(verbose)
    "Finished"
  else
    invisible("Finished")
}


helpr_search_row_count <- 20
#helpr_search_row_count <- 5

get_solr_query_result <- function(query_string){
  rows <- helpr_search_row_count
  response <- urlJSON_to_list(str_join("http://0.0.0.0:8983/solr/select/?version=2.2&wt=json&rows=",rows,"&indent=on&", query_string, "&hl=on&hl.simple.pre=<strong>&hl.simple.post=</strong>&hl.fl=*"))
#  &hl.fl=title_t&hl.fl=desc_t&hl.fl=details_t"))
#  docs <- response$response$docs
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

package_description <- function(pkg, topic){
  gsub("$\n+|\n+^", "", reconstruct(pkg_topic(pkg, topic)$description))
}

search_query_path <- function(query, start_pos){
  str_join(base_html_path(),"/search/start=",start_pos,";q=",query)  
}


helpr_solr_search <- function(query_string){
  result <- get_solr_query_result(query_string)
  items <- result$response
  
  urls <- as.character(names(items))
  desc <- sapply(items, parse_highlighted_desc)
  
  list(
    urls = urls,
    desc = desc,
    items_before = result$items_before,
    items_after = result$items_after,
    query = result$query,
    start_pos = result$items_before,
    row_count = helpr_search_row_count,
    total_item_count = result$total_item_count
  )
  
  
}

parse_highlighted_desc <- function(item){
  item_category <- str_join("<strong>",str_replace(names(item), "_t", ""), ":</strong> ", sep="")
  
  content <- str_join("<td>",item_category, "</td><td>",as.character(item),"</td>", collapse = "</tr><tr>")
  str_join("<table class=\"search_table\"><tr>", content, "</tr></table>", collapse = "")
  
}


index_all_sep <- function(start_letter = "a", verbose = TRUE){
  packages <- suppressWarnings(library()$results[,"Package"])
  packages <- packages[order(tolower(packages))]
  first_letter <- sapply(strsplit(packages, ""), function(x){tolower(x[1])})
  rows <- str_detect(first_letter, str_join("[", tolower(start_letter), "-z]"))
  packages <- packages[rows]
  
  sapply(packages, index_package_sep, verbose = verbose)
}

index_package_sep <- function(package, verbose=TRUE){
  
  suppressWarnings(dir.create("solr"))
  suppressWarnings(dir.create(str_join("solr/", package)))
  require(package, character.only=TRUE)
  
  if(verbose == TRUE)
    cat("\n\n\n")
  if(verbose == TRUE || verbose == "package")
    cat(package,"\n")
  all_topics <- pkg_topics_index(package)
  unique_topics <- all_topics[!duplicated(all_topics$file), "alias"]

  for (i in seq_along(unique_topics)) {
    if(verbose == TRUE)
      cat(i,": ", unique_topics[i],"... ")
    start_time <- Sys.time()
    pkg_output <- solr_topic(package, unique_topics[i])
    time <- Sys.time() - start_time
    if(verbose == TRUE)
      cat("  ", str_sub(capture.output(time), 20), "\n")
      
    save_xml(
      make_add_xml(
        str_join(
          "\n<!--         ", package, "         -->\n", 
          str_join(pkg_output, collapse = "\n\n")
          , collapse = "")
      ),
      str_join("solr/",package,"/",unique_topics[i], ".xml", collapse = "")
    )
  }

}

send_commit_command <- function(){
  send_system_command("curl http://localhost:8983/solr/update --data-binary '<commit/>' -H 'Content-type:text/xml; charset=utf-8'")
}

send_system_command <- function(system_string){
  curled_text <- system(system_string, intern = TRUE, ignore.stderr = TRUE)
  status <- str_sub(curled_text[3], start=47, end=47)
  if(length(status) < 1)
    status <- "FAIL"
  
  if(status != "0"){
    message(str_join(curled_text, collapse = "\n"))
    stop("Error uploading file to solr")
  }
  
}

put_string <- function(string){
  put_file(save_xml(string))
}

put_file <- function(file_name){
  cat("posting file: ", file_name,"\n")
  send_system_command(str_join("curl http://localhost:8983/solr/update --data-binary @", file_name, " -H 'Content-type:text/xml; charset=utf-8'", collapse = ""))
  send_commit_command()
}

#solr_topic
#solr_package
#solr_all
#generate the xml for one topic, all topics in a package, and all packages
