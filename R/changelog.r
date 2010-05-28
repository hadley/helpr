pkg_news <- function(package){
  package <- str_trim(package)
  
  package_news <- news(package = package)
  
  # retain only the latest version infomation
  package_news <- package_news[package_news$Version %in% package_news$Version[1],]
  
  # separate into categories
  cats <- unique(package_news$Category)
  
  returnText <- make_title_wDate("Change Log", unique(package_news$Version) )
  
  for(i in cats){
    
    if(is.na(i))
      rows <- is.na(package_news$Category)
    else
      rows <- package_news$Category == i

    
    returnText <- str_join(
      returnText,
      make_subsection(i), 
      make_paragraph(package_news[rows, "Text"])
    )
  }
  
  returnText

}


make_title_wDate <- function(title, date){
  str_join(
    make_section(title),
    "<span class=\"changelog_date\">",date,"</span>"
  )
}

make_section <- function(text){
  str_join("<h2>",text,"</h2>")
}

make_subsection <- function(text){
  if(!is.na(text))
    str_join("<h3>",text,"</h3>")
  else
    ""
}

make_paragraph <- function(text){
  str_join("<ul><li>",str_join(text, collapse="</li><li>") ,"</li></ul>")  
}