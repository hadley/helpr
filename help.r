library(sinartra)

source("memoise.r")
source("help-parse-rd.r")
source("help-package.r")
source("help-topic.r")
source("help-index.r")

router <- Router$clone()

# Show list of all packages on home page
router$get("/index.html", function() {
  render_brew("index", list(packages = pkg_list()))
})

# Use file in public, if present
router$get("/*", static_file)

# Redirect old home location to new
router$get("/doc/html/index.html", function() {
  redirect("/index.html")
})

router$get("/", function() {
  redirect("/index.html")
})

# If package path, missing trailing /, redirect
router$get("/packages/:package", function(package) {
  redirect(str_join("/packages/", package, "/"))
})

# Package index page, list topics etc
router$get("/packages/:package/", function(package) {
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
router$get("/packages/:package/topics/:topic", function(package, topic) {
  topic_info <- parse_help(pkg_topic(package, topic))
  topic_info$package <- package
  render_brew("topic", topic_info)
})

# Individual help topic 
router$get("/library/:package/html/:topic.html", function(package, topic) {
  redirect(str_join("/packages/", package, "/topics/", topic))
})

render_path <- function(path, ...) router$route(path)
assignInNamespace("httpd", render_path, "tools")
if (tools:::httpdPort == 0L) {
  help.start()
  options("help_type" = "html")
}
