#' Local Mode
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @aliases local_mode activate_internetz deactivate_internetz allow_internetz
#' @keywords internal
local_mode <- local({
  internetz <- TRUE
  deactivate_internetz  <- function() internetz <<- FALSE
  activate_internetz    <- function() internetz <<- TRUE
  allow_internetz       <- function() internetz

  list(
    activate_internetz = activate_internetz, 
    deactivate_internetz = deactivate_internetz, 
    allow_internetz = allow_internetz
  )
})
activate_internetz <- local_mode$activate_internetz
deactivate_internetz <- local_mode$deactivate_internetz
allow_internetz <- local_mode$allow_internetz





#' Router Info
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @aliases rp set_router_info set_fouter_file_path set_router_custom_route router_info fouter_file_path router_custom_route
#' @keywords internal
rp <- local({
  url_path  <- ""
  file_pathP <- getwd()
  custom <- FALSE
  
  set_url           <- function(val)   url_path <<- val
  set_file_path     <- function(val) file_pathP <<- val
  set_custom_router <- function(val)     custom <<- val
    
  url           <- function() url_path
  file_path     <- function() file_pathP
  custom_router <- function() custom
  
  list(
    set_url = set_url, 
    set_file_path = set_file_path, 
    set_custom_router = set_custom_router, 

    url = url, 
    file_path = file_path, 
    custom_router = custom_router
  )
})
set_router_url        <- rp$set_url
set_router_file_path  <- rp$set_file_path
set_router_custom_route <- rp$set_custom_router
router_url            <- rp$url
router_file_path      <- rp$file_path
router_custom_route   <- rp$custom_router


