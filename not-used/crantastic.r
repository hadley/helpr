crantastic_rating <- function(pkg_name){
  txt <- RCurl::getURL(str_join("http://crantastic.org/packages/",pkg_name,"/"))
  rating_location <- str_locate(txt, "overall-rating")[,"start"]

  # retrieve the characters of overall-rating (and some extra)
  subString <- str_sub(txt, start = rating_location, end = rating_location+70)
  
  str_join(str_trim(str_split(subString, "\n")[[1]][2:3]), collapse = "<br />")  
}





rating_text <- function(pkg_name){
  rating_txt <- crantastic_rating(pkg_name)

  # get start position
  rating_value <- as.numeric(str_split(rating_txt,"/")[[1]][1])
  
  star <- "<input name=\"star_rating\" type=\"radio\" class=\"star\" disabled=\"disabled\"/>"
  checked_star <- "<input name=\"star_rating\" type=\"radio\" class=\"star\" disabled=\"disabled\" checked=\"checked\"/>"

  stars_txt <- c(
    rep(star, rating_value - 1),
    checked_star,
    rep(star, 5 - rating_value)
  )
  
  stars_txt <- str_join(stars_txt, collapse = "" )
  cran_txt <- str_join("http://crantastic.org/packages/", pkg_name)
  link_start <- str_join("<a href='",cran_txt,"' target=\"_blank\">")
  
  str_join(link_start, rating_txt, "</a>", "<br />", stars_txt)
}

