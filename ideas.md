Individual pages
================

Main

* WAIT - Main page - display latest changes from CHANGELOG
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
  * should be memoised or hard coded?

Package

* link to CRAN page
* BUSY - links to news & changelogs
* demos
* DONE - vignettes
* NOT FULLY CORRECT - separate data and functions
* NOT FULLY CORRECT - show internal functions separately
* integrate with crantastics
  * DONE - ratings
  * reviews
* show package dependencies

* see CRAN page for more ideas

Topic

* "did you mean?" - include list of all topics with the same name in different packages
  * requires solr
* use latent dirichlet topic models to provide see also links based on description and details.
* display hyperlinked keywords
* view source - shows function source
* rate a function
* add comments - look at something like disqus
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
* BUSY - Evaluate all example code and interleave results back in html, like:
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