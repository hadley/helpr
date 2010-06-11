
get_function_history <- function(){
  rawhist <- NULL
  a <- NULL
  pattern <- "[a-zA-Z_.][a-zA-Z_.0-9]*\\("
  file1 <- tempfile("Rrawhist")
  savehistory(file1)
  rawhist <- readLines(file1)
  unlink(file1)

  # subset then find function names
  rawhist <- grep(pattern, rawhist, value = TRUE )
  funcs <- unlist(str_extract_all(rawhist, pattern))
  funcs <- str_join(funcs, ")")
  funcs <- funcs[ ! funcs %in% c("if()", "for()", "while()", "get_function_history()")]
    
  function_and_link(funcs)
  
}

last_ten_functions <- function(fun_list){
  
  unique_fun_list <- unique(fun_list[rev(seq_len(nrow(fun_list))), ])
  
  last_ten <- min(10, nrow(unique_fun_list))

  unique_fun_list[seq_len(last_ten),]
}

top_ten_functions <- function(fun_list){
  
  func_counts <- table(fun_list$functions)
  top_ten <- min(10, length(func_counts))
  
  func_counts <- func_counts[order(func_counts, decreasing = TRUE)]
  
  func_names <- names(func_counts[1:top_ten])
  paths <- function_help_path(func_names)
  
  as.data.frame(list(functions = func_names, paths = paths), stringsAsFactors = TRUE)
}


ten_functions <- function(){
  fun_hist <- get_function_history()
  
  list(
    top_ten = top_ten_functions(fun_hist), 
    last_ten = last_ten_functions(fun_hist)
  )
}


