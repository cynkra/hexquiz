% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{update_descriptions}
\alias{update_descriptions}
\title{Update description file}
\usage{
update_descriptions(pkgs, path = "inst/www/descriptions.rds")
}
\arguments{
\item{pkgs}{Vector of package names search for.}

\item{path}{Path to save descriptions list to.}
}
\description{
Read the current description and append description from the
input packages. Write back the same file.
}
\keyword{internal}
