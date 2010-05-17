Individual pages
================

Main

* Main page - display latest changes from CHANGELOG
* DONE - Pull in RSS feeds?
  * DONE - needs to be slower
* DONE - Only display loaded packages
* DONE - Display out of date packages
  * DONE - button invoked and highlight
  * needs spinning wheel of death (iphone style)
  * needs install individual button
* BUSY - Button to show all packages
* Top ten functions and most recently used 10 functions
* add links to the manuals

Package

* link to CRAN page
* links to news & changelogs
* demos
* vignettes
* separate data and functions
* show internal functions separately
* integrate with crantastics
  * ratings, reviews

* see CRAN page for more ideas

Topic

* "did you mean?" - include list of all topics with the same name in different packages
  * requires solr
* use latent dirichlet topic models to provide see also links based on description and details.
* display hyperlinked keywords
* view source - shows function source
* DONE - links for "mailto"

Dynamic help
============

Function weights
  * overall CRAN
  * personal function use: parse .Rhistory and pull out functions

Function mispelling

Examples and demos
==================

* Parse examples and add links to functions that are used
* Send selected expression to R console
* Evaluate all example code and interleave results back in html, like:
  http://ejohn.org/apps/learn/

Search
======

* external solr search engine with separate fields for separate parts of RDoc.
* for use by insider and by html search
* indexes all packages - not just those installed on the users machine.

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