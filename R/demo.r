#' Demo Source File
#'
#' @param package package name
#' @param demo_name demo name
#' @return "" if the demo is not found for that package, file.path if the demo folder is found
demo_src_file <- function(package, demo_name){
  system.file("demo", str_join(demo_name, ".R"), package = package)
}

#' Demo Information
#'
#' @param package package name
#' @param demo_name demo name
#' @return demo() information for the \code{demo_name} in \code{package}
# '
demo_info <- function(package, demo_name){
  subset(pkg_demos(package), Item == demo_name)
}

#' Helpr Demo Information
#'
#' @param package package name
#' @param demo_name demo name
helpr_demo <- function(package, demo_name){
  info <- demo_info(package, demo_name)
  
  parsed_src <- parser(file = demo_src_file(package, demo_name))
  
  demo_functions <- code_info(parsed_src)
  other_demos <- suppressWarnings(subset(as.data.frame(pkg_demos(package)), Item != demo_name))
  
  demo_src_c <- demo_src(package, demo_name)

  list(
    package = package, 
    name = demo_name,
    description = info[1,"Title"],
#    src = highlight(parsed_src),
    src = evaluate_text(demo_src_c),
    other_demos = other_demos,
    other_demos_str = pluralize("Demo", other_demos),
    src_functions = demo_functions,
    src_functions_str = pluralize("Top Function", demo_functions)
  )
}

#' Demo Source
#'
#' @param package package name
#' @param demo_name demo name
#' @return source for the demo
demo_src <- function(package, demo_name){
  str_join(readLines(demo_src_file(package, demo_name)), collapse = "\n")
}


#' Package Demos
#'
#' @param package package name
#' @param demo_name demo name
#' @return demo() information for the \code{demo_name} in \code{package}
pkg_demos <- function(package) {
  as.data.frame(demo(package = package)$results, stringsAsFactors = FALSE)
}




