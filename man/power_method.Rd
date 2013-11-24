\name{power_method}
\alias{power_method}
\title{Power Method}
\usage{
  power_method(X, v = NULL, eps = 1e-06, maxiter = 100)
}
\arguments{
  \item{X}{a numeric matrix}

  \item{v}{optional starting vector}

  \item{eps}{convergence threshold}

  \item{maxiter}{maximum number of iterations}
}
\value{
  a list containing the eigenvector, eigenvalue, and
  iterations
}
\description{
  Finds a dominant eigenvalue and its corresponding
  eigenvector of a square matrix by applying the Power
  Method with scaling
}
\examples{
A = cbind(c(2, 1), c(-12, -5))
power_method(A, v = rep(1, nrow(A)))

B = cbind(c(1, -2, 1), c(2, 1, 3), c(0, 2, 1))
power_method(B)
}

