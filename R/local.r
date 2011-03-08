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

