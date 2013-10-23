\name{dummify_matrix}
\alias{dummify_matrix}
\title{Matrix to Dummy Matrix}
\usage{
  dummify_matrix(x, labels = TRUE, full = TRUE)
}
\arguments{
  \item{x}{matrix}

  \item{labels}{should labels for row and columns be
  included?}

  \item{full}{whether all categories are included. When
  \code{full = FALSE} then the first category is removed.}
}
\description{
  Converts a matrix into a dummy matrix
}
\keyword{internal}

