\name{pluralize}
\alias{pluralize}
\title{Pluralize
pluralize a string with either the default 's' according to the boolean statement}
\usage{
  pluralize(string, obj, plural = str_c(string, "s"),
  bool_statement = NROW(obj))
}
\arguments{
  \item{string}{string to be pluralized}

  \item{obj}{object to look at}

  \item{plural}{plural string}

  \item{bool_statement}{boolean to use to determine which
  string to use}
}
\description{
  Pluralize pluralize a string with either the default 's'
  according to the boolean statement
}
\author{
  Barret Schloerke
}
\keyword{internal}

