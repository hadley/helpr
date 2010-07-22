# Installation

To install `helpr`, you first need to install some packages:

  * from CRAN: `install.packages( c("stringr", "digest",  "mutatr", "brew", "testthat") )`
  * from github: `http://github.com/hadley/sinartra`

To install `solr`, you first need to intall `brew`

  * install brew: 
    ruby -e "$(curl -fsS http://gist.github.com/raw/323731/install_homebrew.rb)"
  * install solr with brew: 
    brew install solr
  
# Running helpr

Running from an installed package:

    library(helpr)
    helpr()
  
Running the development version:

    # With working directory set to the helpr package directory
    library(devtools)
    load_all("helpr")
    helpr(FALSE)
    
# Running solr

  * Follow the (http://lucene.apache.org/solr/tutorial.html)[tutorial] to get a feel how it works.
  * Clone the (http://github.com/bigbear/solr_for_helpr)[solr_for_helpr] git repo
  * Execute the command below in the /example solr directory to start the server
    java -jar start.jar 
  * To add/update files, move to /exampledocs and execute the command
    java -jar post.jar filename.xml
  * "*.xml" may be used for all files in the directory with ".xml" ending
  
