\name{inner_prod}
\alias{inner_prod}
\alias{matrix_inner_prod}
\alias{vector_inner_prod}
\title{Inner Product}
\usage{
  inner_prod(x, y, metric = NULL)
}
\arguments{
  \item{x}{numeric vector or matrix}

  \item{y}{numeric vector or matrix}

  \item{metric}{either \code{NULL}, a numeric vector, or a
  numeric square matrix}
}
\description{
  Computes the inner product of two vectors (or two
  matrices) with a specified metric
}
\details{
  \code{x} and \code{y} must be of same length or
  dimension. \cr If \code{metric = NULL} this means the
  identity metric.
}
\examples{
# two vectors
u = 1:10
v = seq(0, 1, length.out = 10)

# identity metric
inner_prod(u, v)

# metric as vector
inner_prod(u, v, metric = rep(1/length(u), length(u)))

# metric as matrix
inner_prod(u, v, metric = diag(1/5, length(v)))

# two matrices
A = matrix(1:12, 4, 3)
B = matrix(seq(0, 1, length.out = 12), 4, 3)

inner_prod(A, B)
inner_prod(A, B, rep(0.3, 4))
inner_prod(A, B, metric = diag(1/2, nrow(A)))
}

