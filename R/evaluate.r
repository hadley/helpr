#' Replay a list of evaluated results.
#' Replay a list of evaluated results, just like you'd run them in a R
#' terminal.
#'
#' @param x result from \code{\link{evaluate}}
#'
helpr_replay <- function(x, pic_base_name) UseMethod("helpr_replay", x)

helpr_replay.list <- function(x, pic_base_name) {
  lapply(seq_along(x), function(i, base_name = pic_base_name){
    item <- x[[i]]
    item_name <- str_join(base_name, "_", i, collapse = "")
    helpr_replay(item, item_name)
  })
}

helpr_replay.character <- function(x, pic_base_name) {
#  helpr_replay_cat(x)
  eval_tag_output(x)
}

helpr_replay.source <- function(x, pic_base_name) {
  helpr_replay_cat(x$src)
}

helpr_replay.warning <- function(x, pic_base_name) {
  helpr_replay_message(str_join("Warning message:\n", x$message, collapse = ""))
}

helpr_replay.message <- function(x, pic_base_name) {
  helpr_replay_message(gsub("\n$", "", x$message))
}

helpr_replay.error <- function(x, pic_base_name) {
  if (is.null(x$call)) {
    helpr_replay_message(str_join("Error: ", x$message, collapse = "\n"))    
  } else {
    call <- deparse(x$call)
    helpr_replay_message(str_join("Error in ", call, ": ", x$message, collapse = "\n"))    
  }
}

helpr_replay.value <- function(x, pic_base_name) {
  if (x$visible) eval_tag_output(str_join(capture.output(print(x$value)), collapse = "\n"))
}

helpr_replay.recordedplot <- function(x, pic_base_name) {  
  file_loc <- save_picture( pic_base_name, x)
  str_join("<img class=\"R_output_image\" src=\"", file_loc, "\" alt=\"", pic_base_name, "\" />", collapse = "")
}

helpr_replay_cat <- function(x){
  if(str_trim(x) == "")
    return(x)

  parsed <- parser(text = x)
  highlight(parsed)
}

helpr_replay_message <- function(x){
  eval_tag_output(str_c("\n<strong>",x,"</strong>"))
}


evaluate_text <- function(txt, pic_base_name){
  if(!has_text(txt))
    return("")
  evaluated <- evaluate:::evaluate(txt, globalenv())
  replayed <- helpr_replay(evaluated, pic_base_name)
  str_join(as.character(unlist(replayed)), collapse = "\n")
}

eval_tag_output <- function(x){
  str_c("<pre class=\"R_output\">", x, "</pre>")
}









## should be replaced with something else
exec_pkg_demo <- function(package, dem) {
#  demo(dem, character = TRUE, package = package, ask = TRUE)
  demo(dem, character = TRUE, package = package, ask = FALSE)
}


# stole from example.  It didn't have the option of character.only
exec_example <- function(package, topic){
    lib.loc = NULL
    local = FALSE
    echo = TRUE
    verbose = getOption("verbose")
    setRNG = FALSE 
    ask = getOption("example.ask")
    prompt.prefix = abbreviate(topic, 6) 
    
    
    
    if (!is.character(topic)) 
        topic <- deparse(topic)[1L]
    pkgpaths <- .find.package(package, lib.loc, verbose = verbose)
    file <- utils:::index.search(topic, pkgpaths, TRUE)
    if (!length(file)) {
        warning(gettextf("no help found for '%s'", topic), domain = NA)
        return(invisible())
    }
    packagePath <- dirname(dirname(file))
    pkgname <- basename(packagePath)
    lib <- dirname(packagePath)
    encoding <- NULL
    tf <- tempfile("Rex")
    encoding <- "UTF-8"
    tools::Rd2ex(utils:::.getHelpFile(file), tf)
    if (!file.exists(tf)) {
        warning(gettextf("'%s' has a help file but no examples", 
            topic), domain = NA)
        return(invisible())
    }
    on.exit(unlink(tf))
    if (pkgname != "base") 
        library(pkgname, lib.loc = lib, character.only = TRUE)
    if (!is.logical(setRNG) || setRNG) {
        if ((exists(".Random.seed", envir = .GlobalEnv))) {
            oldSeed <- get(".Random.seed", envir = .GlobalEnv)
            on.exit(assign(".Random.seed", oldSeed, envir = .GlobalEnv), 
                add = TRUE)
        }
        else {
            oldRNG <- RNGkind()
            on.exit(RNGkind(oldRNG[1L], oldRNG[2L]), add = TRUE)
        }
        if (is.logical(setRNG)) {
            RNGkind("default", "default")
            set.seed(1)
        }
        else eval(setRNG)
    }
    zz <- readLines(tf, n = 1L)
    if (is.null(encoding)) {
        encoding <- if (length(enc <- localeToCharset()) > 1L) 
            c(enc[-length(enc)], "latin1")
        else ""
        if (length(grep("^### Encoding: ", zz)) && !identical(Sys.getlocale("LC_CTYPE"), 
            "C")) 
            encoding <- substring(zz, 15L)
    }
    skips <- 0L
    if (echo) {
        zcon <- file(tf, open = "rt")
        while (length(zz) && !length(grep("^### \\*\\*", zz))) {
            skips <- skips + 1L
            zz <- readLines(zcon, n = 1L)
        }
        close(zcon)
    }
    if (ask == "default") 
        ask <- echo && grDevices::dev.interactive(orNone = TRUE)
    if (ask) {
        if (.Device != "null device") {
            oldask <- grDevices::devAskNewPage(ask = TRUE)
            if (!oldask) 
                on.exit(grDevices::devAskNewPage(oldask), add = TRUE)
        }
        op <- options(device.ask.default = TRUE)
        on.exit(options(op), add = TRUE)
    }
    source(tf, local, echo = echo, prompt.echo = paste(prompt.prefix, 
        getOption("prompt"), sep = ""), continue.echo = paste(prompt.prefix, 
        getOption("continue"), sep = ""), verbose = verbose, 
        max.deparse.length = Inf, encoding = encoding, skip.echo = skips, 
        keep.source = TRUE)
}


OLD_execute_text <- function(text_string){
  in_and_out <- OLD_evaluate_text(text_string)
  for(i in seq_len(NROW(in_and_out))){
    cat("\n> ",in_and_out[i, "input"])
    output <- in_and_out[i, "output"] 
    if(!identical(output, ""))
      cat("\n", output)
  }
  cat("\n> ")
}

OLD_evaluate_text <- function(text_string){
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

OLD_evaluate_file <- function(file){
  source(file)
}
