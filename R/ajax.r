#' Loaded Packages
#' return the packages that are loaded in the current R session
#'
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#'
loaded_packs <- function()
{
  .packages()
}

old_pkg_list <- function() {    
  old_packages <- as.data.frame(old.packages(), stringsAsFactors = FALSE)
  old_packages <- old_packages[order(old_packages$Package), ]
  old_packages <- lapply(1:nrow(old_packages), function(i)
    as.list(old_packages[i, ]))

  old_packages
}

#' Update Old Packages
#' update all packages that are old and currently loaded
#'
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#'
update_loaded_packs <- function()
{
  packs <- loaded_packs()
  packs <- packs[packs %in% old.packages()[,"Package"]]
  install.packages(packs)
  packs
}


#' Print Loaded Packages with Information
#' print the loaded packages in JSON format
#'
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#' @param all boolean of all packages or loaded packages
package_old <- function(all = TRUE)
{
  pack_list <- pkg_list(all)
  
  pack_list <- lapply(pack_list, function(i){
    i$status <- package_status(i$Package)
    i
  })
  
  pack_list
}

#' Package Status
#' return whether the package is up-to-date or not
#'
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#'
package_status <- function(pkg_name){
  status <- pkg_name %in% old.packages()[,"Package"]
  
  if(status)
    "out_of_date"
  else
    "updated"
}
