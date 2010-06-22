## should be replaced with something else
exec_pkg_demo <- function(package, dem) {
#  demo(dem, character = TRUE, package = package, ask = TRUE)
  demo(dem, character = TRUE, package = package, ask = FALSE)
}




execute_text <- function(text_string){
  in_and_out <- evaluate_text(text_string)
  for(i in seq_len(NROW(in_and_out))){
    cat("\n> ",in_and_out[i, "input"])
    output <- in_and_out[i, "output"] 
    if(!identical(output, ""))
      cat("\n", output)
  }
  cat("\n> ")
}

evaluate_text <- function(text_string){
  output <- c()
  input <- parse(text = text_string)
  
  for(i in seq_len(length(input))){
    item <- capture.output(eval(input[i], envir = .GlobalEnv))
    if(length(item) < 1)
      item <- ""
    output[i] <- str_join(item, collapse = "\n")
  }
  
  data.frame(input = as.character(input), output = output, stringsAsFactors = FALSE)
}

evaluate_file <- function(file){
  source(file)
}


