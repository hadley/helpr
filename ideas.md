Individual pages
================

## Main

* WAIT - Main page - display latest changes from CHANGELOG
  * a function page should show all change logs related to that function
  * Needs Change Log
* √ Pull in RSS feeds?
  * √ needs to be slower
* √ Only display loaded packages
* √ Display out of date packages
  * √ button invoked and highlight
  * √ needs spinning wheel of death (iphone style)
  * √ - needs install individual button
* √ Button to show all packages
  * √ needs to 'talk' with old packages button
* √ Rewrite alternative to installed.packages() that is much faster
* √ Top ten functions and most recently used 10 functions
* √ add links to the manuals
  * √ Had coded: should be memoised or hard coded?

## Package

* √ link to CRAN page
* PARTIAL - links to news & changelogs
  * needs to have a render brew template
* √ Link to demos
* √ vignettes
* √ separate data and functions
  * VALIDATION - needs to be able to find functions that are in a namespace, such as the package tools
* √ show internal functions separately
* WAIT - integrate with crantastics
  * √ ratings
  * reviews
* √ show package dependencies
* √ show package suggests
* √ show package imports
* √ show package extends
* √ reverse dependencies

* see CRAN page for more ideas

## Demo

* √ Send to a new page
* See Code Execution
  * √ blocks screen
  * √ performs in Terminal
* √ Have interaction with demo things
  * √ Execute whole demo
  * √ Execute highlighted part
* √ Have output done within the example
  * √ an update from evaluate
* √ Use parser to count all the functions used by the demo.  For functions that are called most offline, add cross-links to the demo from their topic page

## Topic

* DONE? - "did you mean?" - include list of all topics with the same name in different packages
  * handled when "?mutli_pkg_topic" is executed
* DEB - use latent dirichlet topic models to provide see also links based on description and details.
* DEB - display hyperlinked keywords
* √ view source - shows function source
* WAIT - rate a function
  * Crantastic
* √ add comments - look at something like disqus
  * √ add to every page
    * √ demo
    * NO - index.html
    * √ package
    * √ source
    * √ topic
* √ links for "mailto"
* DEB - display keywords
* √ list of all fuctions that are under the topic
  * √ links to the source code of said function
* BUSY - Execute example functionality
  * click button to execute example
  * √ See Code Execution
    

Code Execution
==================

* √ Parse examples and add links to functions that are used
* √ Send selected/highlighted expression to R console
  * √ silently remove "bad" text
    * NEED - to remove the notification
* √ Evaluate all example code and interleave results back in html
  * √ using evaluate package
* √ Add jQuery to notify user
* √ "Easy"(run) button
* √ all executed code should appear in the console
* √ Rewrite the "replay" method for eval with details to produce html output.
  * √ baller status achieved - be cool, make it S3


Dynamic help
============

Function weights
  * ??? - overall CRAN
  * √ personal function use: parse .Rhistory and pull out functions

Function mispelling


Search
======

* √ external solr search engine with separate fields for separate parts of RDoc.
  * NEEDS - external server
* for use by 
  * insider
  * √ html search
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