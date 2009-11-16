source("router.r")

source("help-find.r")
source("help-parse.r")

params <- function(...) list(...)

helper_router <- Router$clone()
helper_router$get("/index.html", function() {
  packages <- as.data.frame(library()$results, stringsAsFactors = FALSE)
  packages <- packages[order(packages$Package), ]
  pkg_list <- lapply(1:nrow(packages), function(i) as.list(packages[i, ]))
  render_brew("index", list(packages = pkg_list))
})

# Static files
helper_router$get("/*", static_file)

helper_router$get("/doc/html/index.html", function() {
  redirect("/index.html")
})

# Package index page, list topics etc
helper_router$get("/packages/:package", function(package) {
  redirect(str_join("/packages/", package, "/"))
})

helper_router$get("/packages/:package/", function(package) {
  topics <- pkg_topics(package)

  info <- .readRDS(system.file("Meta", "package.rds", package = package))
  description <- as.list(info$DESCRIPTION)
  info$DESCRIPTION <- NULL
  description <- defaults(info, description)
  names(description) <- tolower(names(description))
  
  render_brew("package", list(package = package, topics = topics,
    description = description))
})

# Individual help topic
helper_router$get("/packages/:package/topics/:topic", function(package, topic) {
  topic_info <- parse_help(pkg_topic(package, topic))
  topic_info$package <- package
  render_brew("topic", topic_info)
})

# PDF of vignette
helper_router$get("/packages/:package/vignettes/:vignette", params)

render_path <- function(path, ...) helper_router$route(path)
assignInNamespace("httpd", render_path, "tools")


# Function to get information about a package.  List of :
#   * parsed description
#   * help topics
#   * data sets
#   * vignettes

# data_sets <- data(package = pkg)$results[, 3]
# vignettes <- vignette(package=pkg)$results[, 3]
# paths <- sapply(vignettes, function(v) vignette(v, package = pkg)$file)

# Want to rearrange to sets of nested named lists

