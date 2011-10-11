\name{parse_items}
\alias{parse_items}
\title{Parse a list containing "\\item" tags.}
\usage{
  parse_items(rd, package)
}
\arguments{
  \item{rd}{R documentation item to be altered and then
  returned}

  \item{package}{package looking at}
}
\description{
  It will replace the items with plan, non-attributed text.
  It needs to be a 'pre-parser' as it must be done before
  the whole list is reconstructed
}
\author{
  Barret Schloerke
}
\keyword{internal}

