
#' Save a picture
#' save a picture into the temp directory 
#'
#' @return the path to the picture (using the website)
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal 
save_picture <- function(obj_name, obj_plot){
  file_path <- file.path(helpr_pic_path, str_c(obj_name, ".png", collapse = ""))
  
  # only make the picture if you have to
  # duplicates do not exist as naming should be done well
  if(!file.exists(file_path)){ 
    png(file_path)
    on.exit(dev.off())
    print(obj_plot)
  }
  
  str_c("/picture/", obj_name, ".png", collapse = "")
}
