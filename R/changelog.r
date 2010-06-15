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
  
  capture.output(brew::brew("inst/snippets/changelogs.html", change_log))
}

get_manuals <- function(){
  # /Library/Frameworks/R.framework/Resources/doc/manual/
  # get files in the manual directory with full path
  manual_dir <- file.path(Sys.getenv("R_DOC_DIR"),"manual")
  manuals <- dir(manual_dir)
  
  file_loc <- file.path(manual_dir, manuals)
  link <- str_join("manuals/",manuals)
  file_name <- str_replace(manuals, ".html", "")
  
  data.frame(
    title = sapply(file_loc, function(x){
      strip_html(readLines(x, 3)[3])
    }),
    file_loc = file_loc,
    link = link,
    file_name = file_name,
    stringsAsFactors = FALSE
  )
}