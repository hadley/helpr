


helpr_home_content <- function(query){
  query <- str_trim(query)
  return_list <- list()
  if(query == ""){
    return_list <- helpr_home()
    return_list$title <- "Installed packages"
    return_list$water <- "packages"
  }else{
    return_list <- helpr_search(query)
    return_list$title <- "Installed packages"
    return_list$water <- "search"
  }
}





