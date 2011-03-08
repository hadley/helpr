local_mode <- local({
  local_mode <- FALSE
  activate <- function()   local_mode <<- TRUE
  deactivate <- function() local_mode <<- FALSE
  is_active <- function()  local_mode

  list(activate = activate, deactivate = deactivate, is_active = is_active)
})
activate_local <- local_mode$activiate
deactivate_local <- local_mode$deactiviate
local_active <- local_mode$is_active





rp <- local({
  url  <- ""
  base_path <- getwd()
  set_url <- function(val)       url <<- val
  set_base_path <- function(val) base_path <<- val
  
  url <- function()       url
  base_path <- function() base_path
  
  list(set_url = set_url, set_base_path = set_base_path, url = url, base_path = base_path)
})
set_router_url        <- rp$set_url
router_url            <- rp$url
set_router_base_path  <- rp$set_base_path
router_base_path      <- rp$base_path


