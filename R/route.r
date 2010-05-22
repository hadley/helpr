helpr <- function(installed = TRUE) {
  
  if (installed) {
    path <- system.file(package = "helpr")
  } else {    
    path <- normalizePath(file.path(getwd(), "inst"))
  }

  router <- Router$clone()

  # Show list of all packages on home page
  router$get("/index.html", function() {
    render_brew("index", list(packages = as.list(installed_packages())), 
      path = path)
  })

  # Use file in public, if present
  router$get("/*", function(splat) {
    static_file(file.path(path, "public", splat))
  })

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
    
    author_str <- pluralize("Author", description$author)
    
    items <- list(datasets = get_datasets(package), functions = get_functions(topics), idk = get_NOT_FOUND(topics), internal = get_internal(topics))

  
    render_brew("package", list(package = package, items = items,
      description = description, author_str = author_str), path = path)
  })

  # Individual help topic
  router$get("/packages/:package/topics/:topic", function(package, topic) {
    render_brew("topic", topic(package, topic), path = path)
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
  
  # AJAX
  router$get("/packages/old.json", function() {
    render_json(old_package_names())
  })
  router$get("/packages/index.json", function() {
    render_json(installed_packages())
  })
  router$get("/packages/update.json", function() {
    render_json(update_loaded_packs())
  })
  
  return(invisible(router))
}
