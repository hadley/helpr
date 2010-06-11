helpr <- function(installed = TRUE) {
  
  if (installed) {
    path <- system.file(package = "helpr")
  } else {    
    path <- normalizePath(file.path(getwd(), "inst"))
  }

  router <- Router$clone()

  # Show list of all packages on home page
  router$get("/index.html", function() {
    render_brew(
      "index", 
      list(
        packages = as.list(installed_packages()), 
        ten_functions = ten_functions(),
        manuals = get_manuals()
      ), 
      path = path
    )
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
    
    
    package_topics_index <- pkg_topics_index(package)
    items <- get_pkg_topic_type(package)
    
    extras <- list(
      demos = pkg_demos(package),
      vigs = pkg_vigs(package)
    )
    
  
    render_brew(
      "package", 
      list(
        package = package, 
        items = items,
        description = description, 
        author_str = author_str, 
        extras = extras
      ), 
      path = path
    )
    
  })
  
  # Package Demo
  router$get("/packages/:package/demos/:demo", function(package, demo) {
    render_brew("demo", pkg_demo(package, demo), path = path)
  })
  
  # Individual topic source
  router$get("/packages/:package/source/:func", function(package, func) {
    render_brew("source", function_src(package, func), path = path)
  })

  # Individual help topic
  router$get("/packages/:package/topics/:topic", function(package, topic) {
    render_brew("topic", topic(package, topic), path = path)
  })

  # Individual help topic 
  router$get("/library/:package/html/:topic.html", function(package, topic) {
    redirect(str_join("/packages/", package, "/topics/", topic))
  })
  router$get("/library/:package/help/:topic", function(package, topic) {
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
  router$get("/packages/:package/rating.json", function(package) {

#    render_json(rating_text(as.character(package)))
    string <- rating_text(package)
    render_json(string)
  })
  
  # Local Host Files
  router$get("/manuals/:name.html", function(name) {
    file_loc <- as.character(subset(get_manuals(), file_name == name, select = "file_loc"))
    static_file(file_loc)
  })
  
  
  #execute demos
  router$get("/packages/:package/demo/:demo", function(package, demo) {
    exec_pkg_demo(package, demo)
    redirect(str_join("/packages/", package, "/"))
  })

  
  return(invisible(router))
}

