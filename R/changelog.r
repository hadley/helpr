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







#http://cran.r-project.org/manuals.html
#  txt <- RCurl::getURL(str_join("http://crantastic.org/packages/",pkg_name,"/"))
#  rating_location <- str_locate(txt, "overall-rating")[,"start"]
#
#  # retrieve the characters of overall-rating (and some extra)
#  subString <- str_sub(txt, start = rating_location, end = rating_location+70)
#  
#  str_join(str_trim(str_split(subString, "\n")[[1]][2:3]), collapse = "<br />")  

get_manuels <- memoise(function(){
  
  # download manual page and find links
  txt <- RCurl::getURL("http://cran.r-project.org/manuals.html")
  list_locations <- str_locate_all(txt, "<li>")[[1]][,"start"]
  end_loc <- str_locate_all( txt, "</ul>")[[1]][,"start"]
  
  # make start and end points
  list_pos <- data.frame(
    start = c( list_locations), 
    end = c( list_locations[-1], end_loc)
  )
  
  titles_and_links <- t(apply( list_pos, 1, function(row){
    unlist(get_pdf_title_and_link( str_sub( txt, row[1], row[2])))
  }))
  
  titles_and_links
})


get_pdf_title_and_link <- function(txt){
  
  # get the first link
  link_loc <- str_locate(txt, "href=\"")[2] + 1
  
  # get the pos after the '.html'
  end_of_link_pos <- str_locate(str_sub(txt, start = link_loc), "\"")[1] + link_loc - 2
  html_link <- str_sub(txt, start = link_loc, end = end_of_link_pos)
  
  title_start <- str_locate(txt, "<strong>")[2]+1
  title_end <-   str_locate(txt, "</strong>")[1]-1
  title <- str_sub(txt, title_start, title_end)
  
  list(title = title, html_link = html_link)  
}




