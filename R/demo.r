#' Demo source file.
#'
#' @param package package name
#' @param demo_name demo name
#' @return "" if the demo is not found for that package, file.path if the demo folder is found
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
demo_src_file <- function(package, demo_name) {
  system.file("demo", str_c(demo_name, ".R"), package = package)
}

#' Demo information.
#'
#' @param package package name
#' @param demo_name demo name
#' @return demo() information for the \code{demo_name} in \code{package}
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
demo_info <- function(package, demo_name) {
  subset(pkg_demos(package), Item == demo_name)
}

#' helpr demo information.
#'
#' @param package package name
#' @param demo_name demo name
#' @keywords internal
helpr_demo <- function(package, demo_name) {
  info <- demo_info(package, demo_name)
  
  parsed_src <- parser(file = demo_src_file(package, demo_name))
  
  demo_functions <- code_info(parsed_src)
  other_demos <- suppressWarnings(subset(as.data.frame(pkg_demos(package)), Item != demo_name))
  
  demo_src_c <- demo_src(package, demo_name)

  list(
    package = package, 
    name = demo_name,
    description = info[1, "Title"],
    src = highlight(parsed_src),
    other_demos = other_demos,
    other_demos_str = pluralize("Demo", other_demos),
    src_functions = demo_functions,
    src_functions_str = pluralize("Top Function", demo_functions)
  )
}

#' Demo source.
#'
#' @param package package name
#' @param demo_name demo name
#' @return source for the demo
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
demo_src <- function(package, demo_name){
  demo_lines <- readLines(demo_src_file(package, demo_name))
  rows <- str_detect(demo_lines, "example[[:space:]]*\\(")
  demo_lines[rows] <- str_c("# ", demo_lines[rows])
  str_c(demo_lines, collapse = "\n")
}


#' List all demos in a package.
#'
#' @param package package name
#' @return demo() information for the \code{demo_name} in \code{package}
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
pkg_demos <- function(package) {
  as.data.frame(demo(package = package)$results, stringsAsFactors = FALSE)
}
