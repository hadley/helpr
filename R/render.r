#' Render HTML Snippet
#'
#' @param template name of file to use in the folder "views"
#' @param params list containing objects to be sent to the template
#' @param path path to be used to find the "views" folder
#' @return text rendered from the template
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal 
render_snippet <- function(template, params = list(), path = helpr_path()) {
  template <- str_c("_", template)
  render_brew(template, params, path)$payload
}


helpr_path <- memoise(function() {
  if(all(c("DESCRIPTION", "inst", "man", "R") %in% dir() )) {
    if("helpr" %in% dir("../")) {
      return(normalizePath(file.path(getwd(), "inst")))
    }
  }
  system.file(package = "helpr")
})
    
