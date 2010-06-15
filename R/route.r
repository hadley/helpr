helpr_path <- NULL   


helpr <- function(installed = TRUE) {
  if (installed) {
    path <- system.file(package = "helpr")
  } else {    
    path <- normalizePath(file.path(getwd(), "inst"))
  }
  helpr_path <<- path

  router <- Router$clone()

  # Show list of all packages on home page
  router$get("/index.html", function() {
    ten_funcs <- ten_functions()
    render_brew(
      "index", 
      list(
        packages = as.list(installed_packages()), 
        last_ten_funcs_str = pluralize("Last Function", bool_statement = (NROW(ten_funcs$last_ten) > 1), plural = str_join("Last ", NROW(ten_funcs$last_ten), " Functions")),
        last_ten_funcs = ten_funcs$last_ten,
        top_ten_funcs_str = pluralize("Top Function", bool_statement = (NROW(ten_funcs$top_ten) > 1), plural = str_join("Top ", NROW(ten_funcs$top_ten), " Functions")),
        top_ten_funcs = ten_funcs$top_ten,
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

    info <- .readRDS(system.file("Meta", "package.rds", package = package))
    description <- as.list(info$DESCRIPTION)
    info$DESCRIPTION <- NULL
    description <- defaults(info, description)
    names(description) <- tolower(names(description))
    
    author_str <- pluralize("Author", description$author)
    
    package_topics_index <- pkg_topics_index(package)
    topics <- unique(pkg_topics_index(package)$alias)
    
    items <- get_pkg_topic_type(package)
    
    demos <- pkg_demos(package)
    vigs <- pkg_vigs(package)
    
    render_brew(
      "package", 
      list(
        package = package, 
        items = items,
        description = description, 
        author_str = author_str, 
        demos_str = pluralize("Demo", demos),
        demos = demos,
        vigs_str = pluralize("Vignette", vigs),
        vigs = vigs
      ), 
      path = path
    )
    
  })
  
  # Package Vignette
  router$get("/packages/:package/vignette/:vignette", function(package, vignette) {
    static_file(system.file("doc", str_join(vignette, ".pdf"), package = package))
  })

  # Package Demo
  router$get("/packages/:package/demo/:demo", function(package, demo) {
    render_brew("demo", helpr_demo(package, demo), path = path)
  })
  
  # Individual topic source
  router$get("/packages/:package/source/:func", function(package, func) {
    render_brew("source", function_info(package, func), path = path)
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
  router$get("/packages/:package/exec_demo/:demo", function(package, demo) {
    exec_pkg_demo(package, demo)
    redirect(str_join("/packages/", package, "/"))
  })

  
  return(invisible(router))
}

