\name{pseudoinverse}
\alias{pseudoinverse}
\title{matrix pseudo inverse}
\usage{
  pseudoinverse(x, tol)
}
\arguments{
  \item{x}{a numeric matrix}

  \item{tol}{tolerance threshold for singular value
  positiveness}
}
\description{
  computes a pseudo-inverse of a matrix (Moore-Penrose
  inverse)
}
\examples{
set.seed(3)

A = matrix(runif(9), 3, 3)

# pseudo inverse
B1 = pseudoinverse(A)

# A x B1
round(A \%*\% B1, 3)

# compare to 'ginv()'
library(MASS)
B2 = ginv(A)
B2
B1
}

