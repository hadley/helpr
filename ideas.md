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

* DONE - link to CRAN page
* PARTIAL - links to news & changelogs
  * needs to have a render brew template
* DONE - Link to demos
* DONE - vignettes
* DONE - separate data and functions
  * VALIDATION - needs to be able to find functions that are in a namespace, such as the package tools
* DONE - show internal functions separately
* WAIT - integrate with crantastics
  * DONE - ratings
  * reviews
* DONE - show package dependencies
* DONE - show package suggests
* DONE - show package imports
* DONE - show package extends
* DONE - reverse dependencies

* see CRAN page for more ideas

## Demo

* DONE - Send to a new page
* See Code Execution
  * DONE - blocks screen
  * DONE - performs in Terminal
* DONE - Have interaction with demo things
  * DONE - Execute whole demo
  * DONE - Execute highlighted part
* DONE - Have output done within the example
  * DONE - an update from evaluate
* DONE - Use parser to count all the functions used by the demo.  For functions that are called most offline, add cross-links to the demo from their topic page

## Topic

* DONE? - "did you mean?" - include list of all topics with the same name in different packages
  * handled when "?mutli_pkg_topic" is executed
* DEB - use latent dirichlet topic models to provide see also links based on description and details.
* DEB - display hyperlinked keywords
* DONE - view source - shows function source
* WAIT - rate a function
  * Crantastic
* DONE - add comments - look at something like disqus
  * DONE - add to every page
    * DONE - demo
    * NO - index.html
    * DONE - package
    * DONE - source
    * DONE - topic
* DONE - links for "mailto"
* DEB - display keywords
* DONE - list of all fuctions that are under the topic
  * DONE - links to the source code of said function
* BUSY - Execute example functionality
  * click button to execute example
  * DONE - See Code Execution
    

Code Execution
==================

* DONE - Parse examples and add links to functions that are used
* DONE - Send selected/highlighted expression to R console
  * DONE - silently remove "bad" text
    * NEED - to remove the notification
* DONE - Evaluate all example code and interleave results back in html
  * DONE - using evaluate package
* DONE - Add jQuery to notify user
* DONE - "Easy"(run) button
* DONE - all executed code should appear in the console
* DONE - Rewrite the "replay" method for eval with details to produce html output.
  * DONE - baller status achieved - be cool, make it S3


Dynamic help
============

Function weights
  * ??? - overall CRAN
  * DONE - personal function use: parse .Rhistory and pull out functions

Function mispelling


Search
======

* DONE - external solr search engine with separate fields for separate parts of RDoc.
  * NEEDS - external server
* for use by 
  * insider
  * DONE - html search
* WAIT - indexes all packages - not just those installed on the users machine.
  * NEEDS - external server
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