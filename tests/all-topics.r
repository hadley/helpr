parse_all_topics <- function(start_letter = "a"){
  packages <- suppressWarnings(library()$results[,"Package"])
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
      helpr_topic(pkg, unique_topics[i])
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



parse_all_packages <- function(start_letter = "a"){
  packages <- suppressWarnings(library()$results[,"Package"])
  packages <- packages[order(tolower(packages))]
  first_letter <- sapply(strsplit(packages, ""), function(x){tolower(x[1])})
  rows <- str_detect(first_letter, str_join("[", tolower(start_letter), "-z]"))
  packages <- packages[rows]
  slow_pkgs <- data.frame(pkg="", time="", stringsAsFactors = FALSE)
  slow_pos <- 1
  
  for(pkg in packages){
    if(NROW(pkg_topics_index(pkg)) > 0){      
      cat(pkg,": ... ")
      start_time <- Sys.time()
      helpr_package(pkg)
      time <- Sys.time() - start_time
      cat("  ", str_sub(capture.output(time), 20), "\n")
      if(time > 1){
        slow_pkgs[slow_pos, ] <- c(pkg, c(time))
        slow_pos <- slow_pos + 1
      }
    }
  }
  
  slow_pkgs$time <- as.numeric(slow_pkgs$time)
  slow_pkgs
}

parse_all_demos <- function(start_letter = "a"){
  demos <- as.data.frame(demo()$results, stringsAsFactors = FALSE)
  demos <- demos[order(tolower(demos$Item)), ]
  first_letter <- sapply(strsplit(demos$Item, ""), function(x){tolower(x[1])})
  rows <- str_detect(first_letter, str_join("[", tolower(start_letter), "-z]"))
  demos <- demos[rows, ]
  slow_demos <- data.frame(demo="", time="", stringsAsFactors = FALSE)
  slow_pos <- 1
  
  cat("Demos: ", str_join(demos$Item, collapse = ", "), "\n")
  for(i in seq_along(demos$Item)){
    cat(demos[i,"Package"], " - ", demos[i,"Item"],": ... ")
    start_time <- Sys.time()
    helpr_demo(demos[i,"Package"],demos[i,"Item"])
    time <- Sys.time() - start_time
    cat("  ", str_sub(capture.output(time), 20), "\n")
    if(time > 1){
      slow_demos[slow_pos, ] <- c(demos[i,"Item"], c(time))
      slow_pos <- slow_pos + 1
    }
  }
  
  slow_demos$time <- as.numeric(slow_demos$time)
  slow_demos
}