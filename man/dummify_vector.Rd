\name{dummify_vector}
\alias{dummify_vector}
\title{Vector to Dummy Matrix}
\usage{
  dummify_vector(x, labels = TRUE, full = TRUE)
}
\arguments{
  \item{x}{vector}

  \item{labels}{should labels for row and columns be
  included?}

  \item{full}{whether all categories are included. When
  \code{full = FALSE} then the first category is removed.}
}
\description{
  Converts a vector into a dummy matrix
}
\keyword{internal}

