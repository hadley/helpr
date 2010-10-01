\name{helpr_path}
\alias{helpr_path}
\title{Helpr Path: Where's my help at?}
\description{Helpr Path: Where's my help at?}
\value{returns the path the to the helpr dir.  This could be local or the system (installed) path}
\examples{#if(identical(helpr_path(), getwd())) {
#  "you are a developer"
#} else {
#  "you have installed helpr"
#}}
