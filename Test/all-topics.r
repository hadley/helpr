do_all <- function(start_letter = "a"){
  packages <- library()$results[,"Package"]
  packages <- packages[order(tolower(packages))]
  first_letter <- sapply(strsplit(packages, ""), function(x){tolower(x[1])})
  rows <- str_detect(first_letter, str_join("[", tolower(start_letter), "-z]"))
  packages <- packages[rows]
  slow_pkgs <- data.frame(pkg="", topic = "", time="", stringsAsFactors = FALSE)
  slow_pos <- 1
  
  for(pkg in packages){
    cat("\n\n\n", pkg,"\n")
    all_topics <- pkg_topics_index(pkg)
    unique_topics <- all_topics[!duplicated(all_topics$file), "alias"]
    
    for (i in seq_along(unique_topics)) {
      cat(i,": ", unique_topics[i],"... ")
      start_time <- Sys.time()
      topic(pkg, unique_topics[i])
      time <- Sys.time() - start_time
      cat("  ", str_sub(capture.output(time), 20), "\n")
      if(time > 1){
        slow_pkgs[slow_pos, ] <- c(pkg, unique_topics[i], c(time))
        slow_pos <- slow_pos + 1
      }
        
    }
  }
  slow_pkgs$time <- as.numeric(slow_pkgs$time)
  slow_pkgs
}