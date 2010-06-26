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

