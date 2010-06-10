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
    src = highlight(demo_src(info)$src),
    demos = pkg_demos(package, omit = demo),
    src_functions = demo_top_functions(demo)
  )
}

demo_src <- function(demoInfo){
  # just incase the Item changes from the file name rather than a system.file call
  item_path <- file.path(demoInfo["LibPath"], demoInfo["Package"], "demo", str_join(demoInfo["Item"], ".R"))
  src <- str_join(readLines(item_path), collapse = "\n")
  
  list(
    path = item_path,
    src = src
  ) 
}

demo_top_functions <- function(demo){
  src <- demo_src(demo_info(demo))$src
  src_top_functions(src)
}

src_top_functions <- function(text){
  
  funcs_and_paths <- function_and_link(text)
  funcs <- table(funcs_and_paths$functions)
  order <- order(funcs, decreasing = TRUE)
  uni_funs <- unique(funcs_and_paths)
  uni_funs <- uni_funs[order(uni_funs$functions), ]
  funcs <- funcs[order]
  uni_funs <- uni_funs[order, ]

  list(
    name = uni_funs$functions,
    count = funcs,
    link = uni_funs$paths,
    str = pluralize("Top Function", funcs)
  )
}




