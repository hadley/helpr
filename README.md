# helpr

Helpr is an R package that betters friendly HTML documentation. With links to other packages, function aliases, and function sources, finding information is a click away. Using the comprehensive search bar, searching across all R packages is quick and effortless. 

The heart of `helpr` is hosted locally. No internet is required to display all of the documentation, while functionality of the search bar, RSS feed, and comment system requires an internet connection. 

The development of `helpr` was made possible with generous support from Revolution Analytics.

# Main Features

* Home Screen
  * List of all packages loaded / installed
  * Links to all R Manuals
  * Allows you to update out-of-date packages

* Package Page
  * Every help page link is split into groups
  * Links to package vignettes and demos
  * List of general information: Authors, Suggests, Depends, Reverse
    Dependencies, and CRAN info
  * Change log of the latest version
  * Disqus comment system

* Topic Page
  * Display of Description, Usage, Arguments, Details, Value, and Source
  * Usage links to source of each main function
  * List of Authors and “See Also”
  * Examples with full syntax highlighting and able to embed non-interactive
    example output in browser
  * List of top functions used in the examples
  * Disqus comment system

* Source Page
  * Function description
  * Full syntax highlighting
  * List of top functions used in the source
  * Disqus comment system

* Demo Page
  * Demo description
  * Links to other demos within the same package
  * List of top functions used in the demo
  * Full syntax highlighting and able to embed non-interactive demo 
    output in browser
  
# Installation

To install `helpr`, you first need to install some packages:

* from CRAN: `install.packages( c("stringr", "digest",  "mutatr", "brew", "testthat") )`
* from github: `http://github.com/hadley/sinartra`

# Running helpr

Running from an installed package:

    library(helpr)
    helpr()

Running the development version:

    # With working directory set to the helpr package directory
    library(devtools)
    load_all("helpr")
    helpr()
    


# FOR SERVER DEVELOPERS ONLY!!!

Follow the [tutorial](http://lucene.apache.org/solr/tutorial.html) to get a feel how it works.

(Developers) To install `solr`, you first need to install `brew`

  * install brew: `ruby -e "$(curl -fsS http://gist.github.com/raw/323731/install_homebrew.rb)"`
  * install solr with brew: `brew install solr`
  
  * Add/replace the two files in solr-conf to solr's example/solr/conf folder
  * Execute the command `java -jar start.jar` in the /example solr directory to start the server
    * my solr is installed at `/usr/local/Cellar/solr/1.4.0/`
  * To add/update files, execute one of the index commands from R

    `index_topic(package, topic)`
    `index_package(package)`
    `index_all()`

# Design inspiration

* http://www.smashingmagazine.com/2009/06/09/smart-fixes-for-fluid-layouts/
* http://www.sohtanaka.com/web-design/smart-columns-w-css-jquery/
* http://desandro.com/demo/masonry/
* http://cssiphone.com/gallery/
* http://mediaqueri.es/popular/
* https://github.com/davatron5000/FitText.js
* http://trentwalton.com/


