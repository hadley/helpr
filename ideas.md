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
* NEEDS VALIDATION - separate data and functions
  * improved
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
* BUSY - Have output done within the example
  * NEEDS - an update from evaluate
* DONE - Use parser to count all the functions used by the demo.  For functions that are called most offline, add cross-links to the demo from their topic page

## Topic

* WAIT - "did you mean?" - include list of all topics with the same name in different packages
  * requires solr
* DEB - use latent dirichlet topic models to provide see also links based on description and details.
* WAIT/DEB - display hyperlinked keywords
* DONE - view source - shows function source
* WAIT - rate a function
  * Crantastic
* BUSY - add comments - look at something like disqus
  * add to every page
    * DONE - demo
    * NO - index.html
    * DONE - package
    * DONE - source
    * DONE - topic
* DONE - links for "mailto"
* WAIT/DEB - display keywords
* DONE - list of all fuctions that are under the topic
  * DONE - links to the source code of said function
* BUSY - Execute example functionality
  * See Code Execution
    
## Things to fix

* HTML "get selected text" does not include \n or \cr or anything useful for new lines
  * need to get new line characters to allow for executing highlighted code


Code Execution
==================

* DONE - Parse examples and add links to functions that are used
* DONE - Send selected/highlighted expression to R console
  * DONE - silently remove "bad" text
    * NEED - to remove the notification
* BUSY - Evaluate all example code and interleave results back in html
  * see: http://ejohn.org/apps/learn/
  * using evaluate package
* DONE - Add jQuery to notify user
* DONE - "Easy"(run) button
* DONE - all executed code should appear in the console
* DONE - Rewrite the "replay" method for eval with details to produce html output.
  * DONE - baller status achieved - be cool, make it S3


Dynamic help
============

Function weights
  * overall CRAN
  * DONE - personal function use: parse .Rhistory and pull out functions

Function mispelling


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