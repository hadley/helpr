# Installation

To install `helpr`, you first need to install some packages:

  * from CRAN: `install.packages("stringr", "digest",  "mutatr", "brew", "testthat")`
  * from github: `http://github.com/hadley/sinartra`
  
# Running helpr

Running from an installed package:

    library(helpr)
    helpr()
  
Running the development version:

    # With working directory set to the helpr package directory
    library(devtools)
    load_all("helpr")
    helpr(FALSE)