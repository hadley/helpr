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

  render_snippet("changelogs", change_log)
}

function_news <- function(package, topic){
  package_news <- news(package = package)
  if(is.null(package_news)) return(NULL)
  
  # retain only the infomation that contains the topic name
  package_news <- package_news[str_detect(package_news$Text, topic), ]
  
  if(!dataframe_has_rows(package_news)) return("")
    
  package_news$title <- str_join(package_news$Version, " - ", package_news$Category)
  
  change_log <- list(
    title = str_join("Change Log for '", topic, "'", collapse = ""),
    news = split(package_news$Text, addNA(package_news$title))
  )
  
  render_snippet("changelogs_topic_source", change_log)  
}



get_manuals <- function(){
  # get files in the manual directory with full path
  manual_dir <- file.path(Sys.getenv("R_DOC_DIR"),"manual")
  manuals <- dir(manual_dir)
  
  file_loc <- file.path(manual_dir, manuals)
  file_name <- str_replace(manuals, ".html", "")
  link <- str_join("manuals/",file_name, ".html")
  
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