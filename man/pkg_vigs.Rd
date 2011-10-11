\name{pkg_vigs}
\alias{pkg_vigs}
\title{List all package vignettes.}
\usage{
  pkg_vigs(package)
}
\arguments{
  \item{package}{package to explore}
}
\value{
  \code{subset} of the \code{vignette()$results}
  \code{data.frame} ("Package", "LibPath", "Item" and
  "Title")
}
\description{
  List all package vignettes.
}
\author{
  Barret Schloerke
}
\keyword{internal}

