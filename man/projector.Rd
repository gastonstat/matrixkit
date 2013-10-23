\name{projector}
\alias{hat_matrix}
\alias{projector}
\title{Orthogonal Projector (a.k.a. hat matrix)}
\usage{
  projector(x, inverse = "solve")
}
\arguments{
  \item{x}{numeric matrix}

  \item{inverse}{type of inverse. Options: \code{"solve"}
  (default) and \code{"ginv"}}
}
\description{
  produces an orthogonal projection matrix
}
\examples{
# a matrix
set.seed(5)
M = matrix(runif(15), 5, 3)

# projector
projector(M)

# synonym function
hat_matrix(M)
}

