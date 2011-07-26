
#' Make a xml field.
#' make a field for a solr document
#' 
#' @param name name of the field
#' @param value value of the field
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
make_field <- function(name, value) {
  if (length(value) < 1) value <- ""
  value <- str_trim(value)
  value <- gsub("&(?![#]{1})", "&#38;", value, perl=TRUE)
  value <- str_replace_all(value, "<", "&#60;")
  value <- str_replace_all(value, ">", "&#62;")
  value <- str_replace_all(value, "\n", "")
  value <- str_replace_all(value, "\t", "")
  if (!identical(name, "id")) name <- str_c(name, "_t")
  
  str_c("<field name=\"", name, "\">", str_c(value, collapse = "; "),"</field>", collapse = "")
}


#' Turn a list into a solr doc.
#' turn a list into a solr doc
#' 
#' @param id id tag to be used
#' @param obj list to perform on
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
list_to_xml <- function(id, obj) {
  obj$id <- id
  new_obj <- list_to_double_list(obj)

  fields <- sapply(new_obj, function(x) {
    make_field(x$name, x$value)
  })
  
  str_c("<doc>", str_c(fields, collapse=""), "</doc>", collapse = "")
}


#' Make a list into a nested list.
#' this is to be done to easily use sapply and keep the name of the item
#'
#' @param obj list to perform on
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
list_to_double_list <- function(obj) {
  new_obj <- list()
  for(item_name in names(obj)) {
    new_obj[[item_name]] <- list(name = item_name, value = obj[[item_name]])
  }
  new_obj
}


#' Make it so the xml is an 'add'.
#' make it so the xml is an 'add' to be commited to solr
#'
#' @param obj list to perform on
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
make_add_xml <- function(obj) {
  str_c("<add>", obj, "</add>", collaspe = "")
}


#' Save page info.
#' Save page info into xml for solr
#'
#' @param txt xml text string
#' @param file_name location to save the file. Defaults to a temp file that is discarded when R shuts down.
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
save_xml <- function(txt, file_name=tempfile()) {
  txt <- str_replace_all(txt, "<doc>", "<doc>\n\t")
  txt <- str_replace_all(txt, "</field>", "</field>\n\t")
  txt <- str_replace_all(txt, "\t</doc>", "</doc>")
  cat(txt, file = file_name)
  file_name
}


#' PUT a string to the Solr server.
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
put_string <- function(string) {
  file_name <- save_xml(string)
  put_file(file_name)
}

#' PUT a file to the Solr server.
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
put_file <- function(file_name) {
  # require("RCurl")
  cat("posting file: ", file_name,"\n")
  cmd <- str_c("curl ", solr_base_url(), "/solr/update --data-binary @", file_name, " -H 'Content-type:text/xml; charset=utf-8'", collapse = "")
  send_system_command(cmd)
  send_commit_command()
}

