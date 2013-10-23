\name{ones}
\alias{ones}
\alias{ones_matrix}
\alias{ones_vector}
\title{vector and matrix of ones}
\usage{
  ones(n, p = NULL)
}
\arguments{
  \item{n}{indicates either the length (for vector) or the
  number of rows (for matrix)}

  \item{p}{indicates the number of columns (for matrix)}
}
\description{
  \code{ones} produces either a vector or a matrix of ones
  \cr \code{ones_vector} produces a vector of ones of a
  given length \cr \code{ones_matrix} produces a matrix of
  ones of a given dimension
}
\examples{
# vector of ones with 5 elements
ones(5)
# equivalently
ones_vector(5)

# 3x3 matrix of ones
ones(3, 3)
# equivalently
ones_matrix(3, 3)

# 1x3 matrix of ones
ones(1, 3)

# 3x1 matrix of ones
ones(3, 1)
}
\seealso{
  \code{\link{zeros}}
}

