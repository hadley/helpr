demo_info <- function(demo){
  dems <- demo()$results
  dems[dems[,"Item"] == demo, ]
}

pkg_demo <- function(package, demo){
  info <- demo_info(demo)

  list(
    package = package, 
    name = demo,
    info = info,
    src = highlight(demo_src(info)),
    demos = pkg_demos(package, omit = demo),
    src_functions = demo_top_functions(demo)
  )
}

demo_src <- function(demoInfo){
  # just incase the Item changes from the file name rather than a system.file call
  item_path <- file.path(demoInfo["LibPath"], demoInfo["Package"], "demo", str_join(demoInfo["Item"], ".R"))
  str_join(readLines(item_path), collapse = "\n")
}

demo_top_functions <- function(demo){
  src <- demo_src(demo_info(demo))
  code_info(src)
}
