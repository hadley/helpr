#[a-zA-Z_.][a-zA-Z_.0-9]*\\(


#  #taken from history()
#  nlines <- length(rawhist)
#  if (nlines) {
#      inds <- max(1, nlines - max.show):nlines
#      if (reverse) 
#          inds <- rev(inds)
#  }
#  else inds <- integer(0L)
#  file2 <- tempfile("hist")
#  writeLines(rawhist[inds], file2)
#  file.show(file2, title = "R History", delete.file = TRUE)

#  rawhist <- sapply(rawhist, function(row){
#    sub("[\"]", "\"", row)
#  })

#  good_ <- 

  
#  table(functions)

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
  funcs <- funcs[ ! funcs %in% "if()"]
  
  parsed_funcs <- as.data.frame(attributes(parser(text = funcs))$data, stringsAsFactors = FALSE)
  functions <- parsed_funcs$text[parsed_funcs$token.desc == "SYMBOL_FUNCTION_CALL"]
  
  functions <- functions[ ! functions %in% c("get_function_history")]
  paths <- package_function_path(functions)
  
  funcs_and_paths <- as.data.frame(list(functions = functions, paths = paths), stringsAsFactors = FALSE)
  
  funcs_and_paths[complete.cases(funcs_and_paths),]
  
}


package_function_path <- function(func){
  sapply(func, function(x){
    
    tmp <- help(x)[1] 
    if(is.na(tmp)){
      tmp
    }else{
      pos <- str_locate(tmp, "/library/")
      str_sub(tmp, start = pos[1])
    }
  })  
}

is_package_function <- function(func){
  !is.na(package_function_path())
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
#  pkg_function_names <- func_names[is_package_function(func_names)]
  paths <- package_function_path(func_names)
  
  as.data.frame(list(functions = func_names, paths = paths), stringsAsFactors = TRUE)
}


ten_functions <- function(){
  fun_hist <- get_function_history()
  
  list(
    top_ten = top_ten_functions(fun_hist), 
    last_ten = last_ten_functions(fun_hist)
  )
}


