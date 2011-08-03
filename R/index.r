#' Make Helpr topic into xml for solr.
#'
#' @param package package to use
#' @param topic topic to explore
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
solr_topic <- function(package, topic) {
  
  rd <- pkg_topic(package, topic)
  tags <- sapply(rd, tag)

  # Remove top-level text strings - just line breaks between sections
  rd <- rd[tags != "TEXT"]

  out <- list()
  
  # Join together aliases and keywords
  out$Aliases <- setdiff(
    unname(sapply(rd[names(rd) == "alias"], "[[", 1)),
    strip_html(reconstruct(untag(rd$name), package))
  )
  out$Keywords <- unname(sapply(rd[names(rd) == "keyword"], "[[", 1))

  # Title, description, value and examples, need to be stitched into a 
  # single string.
  out$Title <- strip_html(reconstruct(untag(rd$title), package))
  out$Description <- gsub("$\n+|\n+^", "", strip_html(reconstruct(rd$description, package)))
  out$Details <- strip_html(reconstruct(rd$details, package))
  out$Value <- strip_html(reconstruct(rd$value, package))
  out$Authors <- strip_html(reconstruct(rd$author, package))
  out$Package <- str_c(package, " (", pkg_version(package), ")", collapse = "")
  out$Examples <- functions_used(reconstruct(untag(rd$examples), package))

  list_to_xml(
    str_c("/package/", package, "/topic/", topic, collapse = ""),
    out
  )
}

#' Index topic.
#' Index a topic into solr
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @param package package in question
#' @param topic topic in question
index_topic <- function(package, topic) {
  if (! i_can_has_internetz()) return(NULL)

  put_string(make_add_xml(solr_topic(package, topic)))
}


#' Index package.
#' Index a whole package into solr
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @param package package in question
#' @param start_letter used when you want to start froma certain letter, such as 'q'
#' @param verbose should output be shown?
index_package <- function(package, start_letter = "a", verbose = TRUE) {
  if (! i_can_has_internetz()) return(NULL);
  
  if (verbose == TRUE) cat("\n\n\n")
  if (verbose == TRUE || verbose == "package") cat(package,"\n")
  
#  require(package, character.only=TRUE)
  
  all_topics <- pkg_topics_index(package)
  unique_topics <- all_topics[!duplicated(all_topics$file), "alias"]
  
  if (length(unique_topics) > 0) {
    first_letter <- sapply(strsplit(unique_topics, ""), function(x) {tolower(x[1])})
    rows <- str_detect(first_letter, str_c("[", tolower(start_letter), "-z]"))
    unique_topics <- unique_topics[rows]
  }

  pkg_output <- c()
  for (i in seq_along(unique_topics)) {
    if (verbose==TRUE) cat(i,": ", unique_topics[i],"... ")
    start_time <- Sys.time()
    pkg_output[i] <- solr_topic(package, unique_topics[i])
    time <- Sys.time() - start_time
    if (verbose==TRUE) cat("  ", str_sub(capture.output(time), 20), "\n")
  }
  
  # delete all previous information
  solr_delete_package(package)
  
  # add new information
  put_string(
    make_add_xml(
      str_c(
        "\n\n\n<!--         ", package, "         -->\n", 
        str_c(pkg_output, collapse = "\n\n"), 
        collapse = "")
    )
  )

}


#' Index all packages.
#' Index all packages into solr
#'
#' @aliases index_all index_packages
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @param start_letter used when you want to start froma certain letter, such as 'q'
#' @param verbose should output be shown?
index_all <- function(start_letter = "a", verbose = TRUE) {
  if (! i_can_has_internetz()) return(NULL)

  packages <- installed_packages()$Package
  packages <- packages[order(tolower(packages))]
  first_letter <- sapply(strsplit(packages, ""), function(x) { tolower(x[1]) })
  rows <- str_detect(first_letter, str_c("[", tolower(start_letter), "-z]"))
  packages <- packages[rows]
  
  result <- index_packages(packages)

  if (verbose) cat("Finished\n")
  result
}
index_packages <- function(arr) {
  sapply(arr, function(x) {
    tryCatch(
      index_package(x, verbose = verbose),
      error = function(e) {
        "failure"
      }
    )
  })
}