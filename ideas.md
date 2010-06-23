Individual pages
================

## Main

* WAIT - Main page - display latest changes from CHANGELOG
  * a function page should show all change logs related to that function
  * Needs Change Log
* DONE - Pull in RSS feeds?
  * DONE - needs to be slower
* DONE - Only display loaded packages
* DONE - Display out of date packages
  * DONE - button invoked and highlight
  * DONE - needs spinning wheel of death (iphone style)
  * BUSY - needs install individual button
* DONE - Button to show all packages
  * DONE - needs to 'talk' with old packages button
* DONE - Rewrite alternative to installed.packages() that is much faster

* DONE - Top ten functions and most recently used 10 functions
* DONE - add links to the manuals
  * DONE - Had coded: should be memoised or hard coded?

## Package

* link to CRAN page
* PARTIAL - links to news & changelogs
  * needs to have a render brew template
* demos
  * BUSY - Send to a new page
  * See Code Execution
  * Have interaction with demo things
  * Have output done within the example
  * Use parser to count all the functions used by the demo.  For functions that are called most offline, add cross-links to the demo from their topic page
* DONE - vignettes
  * change URL
* NEEDS VALIDATION - separate data and functions
  * improved
  * needs to be able to find functions that are in a namespace, such as the package tools
* NEEDS VALIDATION - show internal functions separately
  * improved
* integrate with crantastics
  * DONE - ratings
  * reviews
* show package dependencies
* reverse dependencies
  * dependsOnPkgs("lattice")
  * see "tools"

* see CRAN page for more ideas

## Topic

* "did you mean?" - include list of all topics with the same name in different packages
  * requires solr
* use latent dirichlet topic models to provide see also links based on description and details.
* display hyperlinked keywords
* view source - shows function source
* rate a function
* add comments - look at something like disqus
* DONE - links for "mailto"
* display keywords
* list of all fuctions that are under the topic
  * links to the source code of said function
    * See Code Execution minus 'run' functionality
    
## Unknown Parse Tags

  * ?checkRd
    * \tabular
    * \tab

## Things to fix

  * \\item
    * does not have a </li>
    * inside a \\describe
    * from ?nlm

Dynamic help
============

Function weights
  * overall CRAN
  * DONE - personal function use: parse .Rhistory and pull out functions

Function mispelling

Examples and demos
==================

* DONE - Parse examples and add links to functions that are used
* BUSY - Send selected/highlighted expression to R console
* BUSY - Evaluate all example code and interleave results back in html, like:
  http://ejohn.org/apps/learn/
* Add jQuery to notify user
* "Easy"(run) button

Search
======

* external solr search engine with separate fields for separate parts of RDoc.
* for use by insider and by html search
* indexes all packages - not just those installed on the users machine.
* lists of functions indexed by keyword


Speed
=====

* Build index of packages as needed, but cache between executions.  
  Use package dates to intelligently invalidate the cache after updates.


Style
=====

Few images, so need creative use of typography.

* Loupe to display function usage
* Use jsmath (http://www.math.union.edu/~dpvc/jsMath/) for nice math display
* Draw inspiration from hannah rd (http://github.com/mislav/hanna), see example at http://gitrdoc.com/mislav/will_paginate/tree/master

* http://24ways.org/2006/compose-to-a-vertical-rhythm
* implement more even line breaks (using Knuth algorithm) for headers with jquery?
* http://www.smashingmagazine.com/2009/01/27/css-typographic-tools-and-techniques/
* http://css-tricks.com/typographic-grid/
* http://rikrikrik.com/jquery/quicksearch/