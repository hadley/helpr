# Render methods to eventually be moved into sinartra

#' Render JSON
#'
#' Produce JSON from an R object
#' @param object object to be turned into JSON
#' @keywords internal 
render_json <- function(object) {
  json <- suppressWarnings(rjson::toJSON(object))
  list(payload = json)
}


#' Render HTML Snippet
#'
#' @param template name of file to use in the folder "views"
#' @param params list containing objects to be sent to the template
#' @param path path to be used to find the "views" folder
#' @return text rendered from the template
#' @keywords internal 
render_snippet <- function(template, params = list(), path = helpr_path){
  if (is.list(params)) {
      env <- new.env(TRUE)
      for (name in names(params)) {
          env[[name]] <- params[[name]]
      }
      params <- env
  }
  path <- file.path(path, "views", str_join("_",template, ".html"))
  if (!file.exists(path)) 
      stop("Can not find ", template, " template ", call. = FALSE)
      
  str_join(capture.output(brew:::brew(path, envir = params)), 
        collapse = "\n")
}