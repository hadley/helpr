# Render methods to eventually be moved into sinartra

render_json <- function(object) {
  json <- rjson::toJSON(object)
  list(payload = json)
}


render_snippet <- function(template, params, path = helpr_path){
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
      
  str_join(capture.output(brew(path, envir = params)), 
        collapse = "\n")
}