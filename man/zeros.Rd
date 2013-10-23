\name{zeros}
\alias{zeros}
\alias{zeros_matrix}
\alias{zeros_vector}
\title{vector and matrix of zeros}
\usage{
  zeros(n, p = NULL)
}
\arguments{
  \item{n}{indicates either the length (for vector) or the
  number of rows (for matrix)}

  \item{p}{indicates the number of columns (for matrix)}
}
\description{
  \code{zeros} produces either a vector or a matrix of
  zeros \cr \code{zeros_vector} produces a vector of zeros
  of a given length \cr \code{zeros_matrix} produces a
  matrix of zeros of a given dimension
}
\examples{
# vector of zeros with 5 elements
zeros(5)
# equivalently
zeros_vector(5)

# 3x3 matrix of zeros
zeros(3, 3)
# equivalently
zeros_matrix(3, 3)

# 1x3 matrix of zeros
zeros(1, 3)

# 3x1 matrix of zeros
zeros(3, 1)
}
\seealso{
  \code{\link{ones}}
}

