\name{dummify_factor}
\alias{dummify_factor}
\title{Factor to Dummy Matrix}
\usage{
  dummify_factor(x, labels = TRUE, full = TRUE)
}
\arguments{
  \item{x}{factor}

  \item{labels}{should labels for row and columns be
  included?}

  \item{full}{whether all categories are included. When
  \code{full = FALSE} then the first category is removed.}
}
\description{
  Converts a factor into a dummy matrix
}
\keyword{internal}

