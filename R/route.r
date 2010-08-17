#' load html pages
#' load an html page from the console
#' 
#' @param ... site to be loaded
load_html <- function(...) {
  url_path <- str_c(as.character(substitute(...)), collapse = "/")

  if(str_sub(url_path, end = 1) != "/") {
    url_path <- str_c("/", url_path, collapse = "")
  }

  if(str_sub(url_path, end = 2) == "//") {
    url_path <- str_sub(url_path, start = 2)
  }
  
  browseURL(paste(base_html_path(), url_path, sep = ""), getOption("browser"))
}

#' base html path
#' returns the base html path needed to load a website
#' 
#' @param ... site to be loaded
#' @keyword internal
base_html_path <- function() {
  str_c("http://127.0.0.1:", tools:::httpdPort, collapse = "")
}

#' check for a package
#' check to see if a package exists
#' 
#' @param package package in question
#' @keyword internal
#' @examples
#'   check_for_package("stats")
#'   check_for_package("does_not_exist")
check_for_package <- function(package){
  package %in% installed_packages()$Package
}


# Global path for render_snippets and pictures
helpr_path <- NULL
helpr_pic_path <- base::tempdir()
   

#' Helpr Documentation
#'
#' Execute to show documentation
#' @param installed use TRUE if the package is installed on from CRAN
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
    render_brew("index", helpr_home(), path = path)
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
  router$get("/package/:package", function(package) {
    redirect(str_c("/package/", package, "/"))
  })

  # Package index page, list topics etc
  router$get("/package/:package/", function(package) {
    if(check_for_package(package)){
      render_brew("package", helpr_package(package), path = path)    
    } else {
      render_brew("whistle", list(package = package, url = deparse(str_c("/package/", package, "/", collapse = ""))), path = path)      
    }
  })
  
  # Package Vignette
  router$get("/package/:package/vignette/:vignette", function(package, vignette) {
    if(check_for_package(package)){
      static_file(system.file("doc", str_c(vignette, ".pdf"), package = package))
    } else {
      render_brew("whistle", list(package = package, url = deparse(str_c("/package/", package, "/vignette/", vignette, collapse = ""))), path = path)      
    }
  })

  # Package Demo
  router$get("/package/:package/demo/:demo", function(package, demo) {
    if(check_for_package(package)){
      render_brew("demo", helpr_demo(package, demo), path = path)
    } else {
      render_brew("whistle", list(package = package, url = deparse(str_c("/package/", package, "/demo/", demo, collapse = ""))), path = path)      
    }
  })
  
  # Individual topic source
  router$get("/package/:package/topic/:topic/source/:func", function(package, topic, func) {
    if(check_for_package(package)){
      render_brew("source", helpr_function(package, func), path = path)
    } else {
      render_brew("whistle", list(package = package, url = deparse(str_c("/package/", package, "/topic/", topic, "/source/", func, collapse = ""))), path = path)      
    }
  })

  # Individual help topic
  router$get("/package/:package/topic/:topic", function(package, topic) {
    if(check_for_package(package)){
      render_brew("topic", helpr_topic(package, topic), path = path)
    } else {
      render_brew("whistle", list(package = package, url = deparse(str_c("/package/", package, "/topic/", topic, collapse = ""))), path = path)      
    }
  })
  router$get("/library/:package/html/:topic.html", function(package, topic) {
    redirect(str_c("/package/", package, "/topic/", topic))
  })
  router$get("/library/:package/help/:topic", function(package, topic) {
    redirect(str_c("/package/", package, "/topic/", topic))
  })

  # Individual help topic with multiple reponses
  router$get("/multiple_help_paths/:path_string", function(path_string) {
    pkg_and_topic <- str_split(str_split(path_string, ";")[[1]], "~")

    pkg <- c()
    topic <- c()
    descriptions <- c()
    for(i in seq_along(pkg_and_topic)){
      pkg[i] <- pkg_and_topic[[i]][1]
      topic[i] <- pkg_and_topic[[i]][2]
      descriptions[i] <- package_description(pkg[i], topic[i])
    }
    
    render_brew("help", list(pkg = pkg, topic = topic, desc = descriptions), path = path)
  })
  
  # Search
  router$get("/search/:query_string", function(query_string) {
    query_string <- str_replace(query_string, ";", "&")
    render_brew("search", helpr_solr_search(query_string), path = path)  
  })
  
  
  
  # AJAX
  router$get("/package/old.json", function() {
    render_json(old_package_names())
  })
  router$get("/package/index.json", function() {
    render_json(installed_packages())
  })
  router$get("/package/update.json/:all_packs", function(all_packs) {
    render_json(update_packs(all_packs))
  })
  router$get("/package/install.json/:pkg", function(pkg) {
    install_packages(pkg)
    render_json(TRUE)
  })
  router$get("/package/:package/rating.json", function(package) {
    string <- rating_text(package)
    render_json(string)
  })
  router$get("/package/:package/exec_demo/:demo", function(package, demo) {
    exec_pkg_demo(package, demo)
    render_json(TRUE)
  })
  router$get("/package/:package/topic/:topic/exec_example", function(package, topic) {
    exec_example(package, topic)
    render_json(TRUE)
  })

  
  
  # Manual HTML Files
  router$get("/manuals/:name.html", function(name) {
    file_loc <- as.character(subset(get_manuals(), file_name == name, select = "file_loc"))
    static_file(file_loc)
  })
  
  
  
  #execute code  
  router$get("/eval_text/*", function(splat){
    decoded_text <- URLdecode(splat)
    cat("\n")
    evaluate:::replay.list(evaluate:::evaluate(decoded_text, envir = .GlobalEnv))
    cat(options("prompt")$prompt)

    render_json(TRUE)
  })
  router$get("/eval_demo/:package~:demo_name", function(package, demo_name){
    list(payload = evaluate_text(demo_src(package, demo_name), pic_base_name = str_c(package, "_", pkg_version(package),"_demo_", demo_name)))
  })

  router$get("/eval_example/:package~:topic", function(package, topic){
    list(payload = evaluate_text(reconstruct(untag(pkg_topic(package, topic)$examples)), pic_base_name = str_c(package, "_", pkg_version(package),"_topic_", topic)))
  })
  

  # Individual help topic 
  router$get("/g", function() {
    redirect("package/stats/demo/glm.vr")
  })
  router$get("/n", function() {
    redirect("package/stats/topic/nlm")
  })
  

  # pictures
  router$get("/picture/:file_name", function(file_name) {
    file_path <- file.path(helpr_pic_path, file_name)
    static_file(file_path)
  })

  render_path <- function(path, ...) router$route(path)
  assignInNamespace("httpd", render_path, "tools")
  if (tools:::httpdPort == 0L) {
    help.start()
    options("help_type" = "html")
  } else if(FALSE){
    # load the home page
    load_html()
  }

  return(invisible(router))
}


#' Helpr Home
#'
#' @return all the information necessary to produce the home site ("index.html")
helpr_home <- function(){

  ten_funcs <- ten_functions()

  last_ten_length <- NROW(ten_funcs$last_ten)
  top_ten_length <- NROW(ten_funcs$top_ten)

  list(
    packages = installed_packages(), 
  
    last_ten_funcs_str = pluralize(
      "Last Function", 
      bool_statement = (last_ten_length > 1), 
      plural = str_c("Last ", last_ten_length, " Functions")
    ),
    last_ten_funcs = ten_funcs$last_ten,
    top_ten_funcs_str = pluralize(
      "Top Function", 
      bool_statement = (top_ten_length > 1), 
      plural = str_c("Top ", top_ten_length, " Functions")
    ),
    top_ten_funcs = ten_funcs$top_ten,
    manuals = get_manuals()
  )
}

#' help files with topic for helpr
#' route to the correct website
#'
#' function taken from utils
#'
#' @param x path to help
#' @param ... other arguments ignored
print.help_files_with_topic <- function (x, ...) 
{
    browser <- getOption("browser")
    topic <- attr(x, "topic")
    type <- attr(x, "type")
    paths <- as.character(x)
    if (!length(paths)) {
        writeLines(c(gettextf("No documentation for '%s' in specified packages and libraries:", 
            topic), gettextf("you could try '??%s'", topic)))
        return(invisible(x))
    }
    if (attr(x, "tried_all_packages")) {
        paths <- unique(dirname(dirname(paths)))
        msg <- gettextf("Help for topic '%s' is not in any loaded package but can be found in the following packages:", 
            topic)
        if (type == "html" && tools:::httpdPort > 0L) {
            path <- file.path(tempdir(), ".R/doc/html")
            dir.create(path, recursive = TRUE, showWarnings = FALSE)
            out <- paste("<!DOCTYPE html PUBLIC \"-//W3C//DTD HTML 4.01 Transitional//EN\">\n", 
                "<html><head><title>R: help</title>\n", "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=\"UTF-8\">\n", 
                "<link rel=\"stylesheet\" type=\"text/css\" href=\"/doc/html/R.css\">\n", 
                "</head><body>\n\n<hr>\n", sep = "")
            out <- c(out, "<p>", msg, "</p><br>")
            out <- c(out, "<table width=\"100%\" summary=\"R Package list\">\n", 
                "<tr align=\"left\" valign=\"top\">\n", "<td width=\"25%\">Package</td><td>Library</td></tr>\n")
            pkgs <- basename(paths)
            links <- paste("<a href=\"", base_html_path(), 
                "/library/", pkgs, "/help/", topic, "\">", pkgs, 
                "</a>", sep = "")
            out <- c(out, paste("<tr align=\"left\" valign=\"top\">\n", 
                "<td>", links, "</td><td>", dirname(paths), "</td></tr>\n", 
                sep = ""))
            out <- c(out, "</table>\n</p>\n<hr>\n</body></html>")
            writeLines(out, file.path(path, "all.available.html"))
            browseURL(paste(base_html_path(), 
                "/doc/html/all.available.html", sep = ""), browser)
        }
        else {
            writeLines(c(strwrap(msg), "", paste(" ", formatDL(c(gettext("Package"), 
                basename(paths)), c(gettext("Library"), dirname(paths)), 
                indent = 22))))
        }
    }
    else {
        if (tools:::httpdPort == 0L) 
            tools::startDynamicHelp()
        file <- paths
        pkgname <- basename(dirname(dirname(file)))
        topic <- basename(file)

        if(length(pkgname) > 1){
          browseURL(paste(base_html_path(), 
          "/multiple_help_paths/", str_c(pkgname, topic, sep = "~", collapse = ";"), sep = ""), browser)
        }else{
          browseURL(paste(base_html_path(), 
          "/library/", pkgname, "/html/", topic, 
          ".html", sep = ""), browser)            
        }
    }
    invisible(x)
}

