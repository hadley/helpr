#' @include memoise.r

pkg_matrix <- function(){
  libraryResults <- as.data.frame(library()$results, stringsAsFactors = FALSE)

  # subset to packages that are loaded
  
  packages <- libraryResults

  packages$isLoaded <- packages$Package %in% loaded_packs()
    
  packages <- packages[order(packages$Package), ]
  packages  
}

pkg_dlply <- function(pkgs){
  lapply(1:nrow(pkgs), function(i) as.list(pkgs[i, ]))  
}

pkg_list <- function() {
  pkg_dlply(pkg_matrix())
}

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
  pkg_dlply(old_packages)
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
#' 
package_old <- function()
{
  pack_matrix <- pkg_matrix()
  pack_status <- pack_matrix$Package %in% old.packages()[,"Package"]
  pack_matrix$status <- as.character(factor(pack_status, labels = c("updated", "out_of_date")))
  pkg_dlply(pack_matrix)
}

#' Package Status
#' return whether the package is up-to-date or not
#'
#' @author Barret Schloerke \email{bigbear@@iastate.edu}
#'
#package_status <- function(pkg_name){
#  status <- pkg_name %in% old.packages()[,"Package"]
#  
#  if(status)
#    "out_of_date"
#  else
#    "updated"
#}
