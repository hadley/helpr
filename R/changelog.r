pkg_news <- function(package){
  package_news <- news(package = package)
  if(is.null(package_news)) return(NULL)
  
  # retain only the latest version infomation
  latest <- package_news$Version[1]
  package_news <- package_news[package_news$Version %in% latest, ]
  
  change_log <- list(
    title = "Change Log",
    date = unique(package_news$Version),
    news = split(package_news$Text, addNA(package_news$Category))
  )
  
  capture.output(brew("inst/snippets/changelogs.html", change_log))
}

get_manuals <- function(){
  # /Library/Frameworks/R.framework/Resources/doc/manual/
  # get files in the manual directory with full path
  manual_dir <- str_join(Sys.getenv("R_DOC_DIR"),"manual", sep = .Platform$file.sep)
  html_files <- str_join(manual_dir, dir(manual_dir), sep=.Platform$file.sep)
  
  list(
    html_files = str_join("/_",html_files),
    titles = sapply(html_files, function(x){
      strip_html(readLines(x, 3)[3])
    })
  )
}