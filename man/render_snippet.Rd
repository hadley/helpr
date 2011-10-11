\name{render_snippet}
\alias{render_snippet}
\title{Render HTML Snippet.}
\usage{
  render_snippet(template, params = NULL, path =
  router_file_path())
}
\arguments{
  \item{template}{name of file to use in the folder
  "views"}

  \item{params}{list containing objects to be sent to the
  template}

  \item{path}{path to be used to find the "views" folder}
}
\value{
  text rendered from the template
}
\description{
  Render HTML Snippet.
}
\author{
  Barret Schloerke
}
\keyword{internal}

