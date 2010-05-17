old_pkg_list <- function() {
  if(!require(rjson))
    return(c("null"))
    
  old_packages <- as.data.frame(old.packages(), stringsAsFactors = FALSE)
  old_packages <- old_packages[order(old_packages$Package), ]
  old_packages <- lapply(1:nrow(old_packages), function(i) as.list(old_packages[i, ]))
  a <- rjson::toJSON(old_packages)
#  browser()
  cat(a)
  a
  
}