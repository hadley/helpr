\name{parse_items}
\alias{parse_items}
\title{Parse List with Items...}
\usage{parse_items(rd, package)}

\description{
  Parse List with Items parse a list containing "\\item" tags.
}

\details{
  It will replace the items with plan, non-attributed text.  It needs
  to be a 'pre-parser' as it must be done before the whole list is
  reconstructed
}
\author{Barret Schloerke \email{schloerke@gmail.com}}
\keyword{internal}
\arguments{
  \item{rd}{R documentation item to be altered and then returned}
  \item{package}{package looking at}
}
