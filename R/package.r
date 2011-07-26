#' Package information.
#'
#' @aliases helpr_package helpr_package_mem
#' @return all the information necessary to produce a package site ("/package/:package/")
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
helpr_package <- function(package) {
  helpr_package_mem(package, pkg_version(package))
}

helpr_package_mem <- memoise(function(package, version) {
  
  info <- readRDS(system.file("Meta", "package.rds", package = package))
  description <- as.list(info$DESCRIPTION)
  info$DESCRIPTION <- NULL
  description <- modifyList(info, description)
  names(description) <- tolower(names(description))
  
  author_str <- pluralize("Author", description$author)
  
  items <- pkg_topics_alias(package)
  
  demos <- pkg_demos(package)
  vigs <- pkg_vigs(package)

  description$depends <- parse_pkg_desc_item(description$depends)
  description$imports <- parse_pkg_desc_item(description$imports)
  description$suggests <- parse_pkg_desc_item(description$suggests)
  description$extends <- parse_pkg_desc_item(description$extends)
  description$reverse <- dependsOnPkgs(package)
  description$author <- pkg_author_and_maintainers(description$author, description$maintainer)
  description$maintainer <- NULL

  if (has_text(description$url)) {
    description$url <- str_trim(str_split(description$url, ",")[[1]])
  }

  list(
    package = package, 
    items = items,
    description = description, 
    author_str = author_str, 
    demos_str = pluralize("Demo", demos),
    demos = demos,
    vigs_str = pluralize("Vignette", vigs),
    vigs = vigs,
    change_log = pkg_news(package)
  )
})

#' Package version from the rd file
#'
#' @keywords internal
#' @author Hadley Wickham
pkg_version <- function(pkg) {
  rds <- readRDS(system.file("Meta", "package.rds", package = pkg))
  rds$DESCRIPTION[["Version"]]
}


#' Add package link to a string.
#' 
#' @param string string to be evaluated
#' @keywords internal
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @examples #add_package_link_to_string("quantreg, Hmisc, mapproj, maps, hexbin, gpclib, maptools")
add_package_link_to_string <- function(string) {
	packs <- str_trim(str_split(usage_functions(string), "[, ]")[[1]])
	packs <- packs[packs != ""]
	pack_link <- str_c("<a href='", router_url(), "/package/", packs, "/' >", packs, "</a>")

	for(i in seq_along(packs)){
		string <- str_replace(string, packs[i], pack_link[i])[[1]]
	}
	string
}

#' Ensure package version is properly displayed (if not already in a nice
#' format).
#'
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
parse_pkg_desc_item <- function(obj) {
  if (NROW(obj) < 1) {
    return(NULL)
  }
  
	if (is.character(obj)) {
		return(obj)
	}
	
  if (!is.list(obj)) {
    obj <- list(obj = (list(name = obj, version = NULL)))
  }

  as.data.frame(
    sapply(obj, function(x) {
      vers <- NULL
      
      # if the version is found, it will create one in the form of '(1.2.3)'
      if (!is.null(x$version)) {
        vers <- str_c("(", x$op, " ", str_c(unclass(x$version)[[1]], collapse = "."), ")", collapse = "")
      }
      list(name = as.character(x$name), version = as.character(vers))
    })
    , stringsAsFactors = FALSE
  )
}

#' Parse authors and maintainer.
#'
#' @param description list containing the \code{author} and \code{maintainer}
#' @return string containing the authors with links properly displayed
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
pkg_author_and_maintainers <- function(authors, maintainer=NULL) {

  # retrieve the authors and email
  authors <- str_replace_all(authors, "\\n", " ")
  str_extract_all(authors, "[a-zA-z]* [a-zA-z]* <*>")

  # retrieve the author and email separately
  all_pattern <- "[a-zA-Z][a-zA-Z]* [a-zA-Z][a-zA-Z]* <[a-zA-Z0-9._-]*@[a-zA-Z0-9._-]*.[a-zA-Z]{2,7}>"
  name_pattern <- "[a-zA-Z][a-zA-Z_-]*[ ]{0,1}[a-zA-Z][a-zA-Z]*"
  email_pattern <- "<[a-zA-Z0-9._-]*@[a-zA-Z0-9._-]*.[a-zA-Z]{2,7}>"
  auths_string <- str_extract_all(authors, all_pattern)[[1]]
  auths <- sapply(str_extract_all(auths_string, name_pattern), "[[", 1)
  emails <- sapply(str_extract_all(auths_string, email_pattern), "[[", 1)
  if (length(emails) < 1) {
    author_and_email <- auths
  } else {
    emails <- str_replace_all(emails, "<", "")
    emails <- str_replace_all(emails, ">", "")
    
    author_and_email <- author_email(auths, emails)
  }
  
  # replace the original authors with the linked authors
  for (i in seq_along(author_and_email)) {
    authors <- str_replace_all(authors, auths_string[i], author_and_email[i])
  }

  if (!is.null(maintainer)) {
    maintainer_name <- str_trim(strip_html(maintainer))
    maintainer_email <- str_extract_all(maintainer, email_pattern)[[1]][1]
    maintainer_email <- str_replace_all(maintainer_email, "<", "")
    maintainer_email <- str_replace_all(maintainer_email, ">", "")
  
    # make the maintainer bold
    maintainer_string <- str_c("<strong>", author_email(maintainer_name, maintainer_email), "</strong>", collapse = "")  
  
    if (str_detect(authors, maintainer_name)) {
      # replace the current author text with the maintainer text
      authors <- str_replace_all(
        authors, 
        str_c("</?.*?>",maintainer_name,"</?.*?>", collapse = ""),
        maintainer_name
      )
      
      authors <- str_replace_all(
        authors, 
        maintainer_name,
        maintainer_string
      )
    } else {
      # attach the maintainer to the end
      authors <- str_c(authors, "; ", maintainer_string, collapse = "")
    }
  }
  authors
}

#' Documentation database path.
#'
#' @param package package to explore
#' @return \code{file.path} to the documentation database
#' @keywords internal
#' @author Hadley Wickham
pkg_rddb_path <- function(package) {
  file.path(pkg_help_path(package), package)
}

#' Package help path.
#'
#' @param package package to explore
#' @return \code{file.path} to the help folder
#' @keywords internal
#' @author Hadley Wickham
pkg_help_path <- function(package) {
  system.file("help", package = package)
}


#' List all package vignettes.
#'
#' @param package package to explore
#' @return \code{subset} of the \code{vignette()$results} \code{data.frame} ("Package", "LibPath", "Item" and "Title")
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
pkg_vigs <- function(package) {
  vignettes <- vignette(package = package)$results
  
  if (!NROW(vignettes)) {
    return(NULL)
  }

  titles <- str_replace_all(vignettes[,4], "source, pdf", "")
  titles <- str_trim(str_replace_all(titles, "[()]", ""))
  
  data.frame(item = vignettes[,"Item"], title = titles, stringsAsFactors = FALSE)
}

#' Package topics alias to file index.
#'
#' @param package package to explore
#' @return \code{\link{data.frame}} containing \code{alias} (function name) and \code{file} that it is associated with
#' @keywords internal
#' @author Hadley Wickham
pkg_topics_index <- memoise(function(package) {
  help_path <- pkg_help_path(package)
  
  file_path <- file.path(help_path, "AnIndex")
  ### check to see if there is anything that exists, aka sinartra
  if (length(readLines(file_path, n = 1)) < 1) {
    return(NULL)
  }

  topics <- read.table(file_path, sep = "\t", 
    stringsAsFactors = FALSE, comment.char = "", quote = "", header = FALSE)
    
  names(topics) <- c("alias", "file") 
  topics[complete.cases(topics), ]
})


#' Package topics file documentation.
#'
#' Items can be accessed by \emph{\code{list()}}\code{$file_name}
#' @param package package to explore
#' @return \code{\link{list}} containing the documentation file of each file of a package
#' @keywords internal
#' @author Hadley Wickham
pkg_topics_rd <- memoise(function(package) {
  rd <- tools:::fetchRdDB(pkg_rddb_path(package))
  lapply(rd, name_rd)
})

#' Topic title and aliases by package.
#' return information on the package, datasets, internal, and datasets
#'
#' @param pkg package in question
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
pkg_topics_alias <- function(pkg) {
    
  rd1 <- pkg_topics_rd(pkg)
  rd <- lapply(rd1, function(x) {
    desc <- reconstruct(untag(x$description), pkg)
    desc_naked <- strip_html(desc)
    if (str_length(desc_naked) > 150) {
      desc <- str_c(str_sub(desc_naked, end = 150), " ...")
    }

    list(
      topic = unlist(x$name),
      alias = unname(sapply(x[names(x) == "alias"], "[[", 1)),
      keywords = str_trim(reconstruct(untag(x$keyword), pkg)),
      desc = desc,
      title = reconstruct(untag(x$title), pkg)
    )
  })

  keywords <- sapply(rd, function(x){ x$keywords })
          
  package_info <- rd[keywords == "package"]
  internal <- rd[keywords == "internal"]
  dataset <- rd[keywords == "datasets"]  
  
  rows <- keywords %in% c("package", "internal", "datasets")

  if (sum(rows) > 0) rd[rows] <- NULL 
  
  list(func = rd, dataset = dataset, internal = internal, info = package_info)
}

#' Package description
#'
#' @param pkg package in question
#' @param topic topic in question
#' @author Barret Schloerke \email{schloerke@@gmail.com}
#' @keywords internal
package_description <- function(pkg, topic) {
  gsub("$\n+|\n+^", "", reconstruct(pkg_topic(pkg, topic)$description, package))
}


