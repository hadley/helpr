
save_picture <- function(obj_name, obj_plot){
  file_path <- file.path(helpr_pic_path, str_join(obj_name, ".png", collapse = ""))
  
  # only make the picture if you have to
  # duplicates do not exist as naming should be done well
  if(!file.exists(file_path)){ 
    cat("Saving picture... ", obj_name, "\n\t", file_path, "\n",sep = "")
    png(file_path)
      print(obj_plot)
    dev.off()
  }
  
  str_join("/picture/", obj_name, ".png", collapse = "")
}






delete_folder_contents <- function(folder)
{    
#  setwd(paste(folder, "/", sep="", collapse=""))
  
  files_in_folder <- file.path(folder, dir(folder, recursive = TRUE))

  if(length(files_in_folder) > 0){
    cat("Deleting files\n"); print(files_in_folder)
    file.remove(files_in_folder)
  }
  
  folders <- file.path(folder, dir(folder))
  for(j in seq_along(folders)){
    successful <- file.remove(folders[j])
    if(! successful){
      delete_folder_contents(folders[j])
      file.remove(folders[j])
    }
  }


}
