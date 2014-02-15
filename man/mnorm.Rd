\name{mnorm}
\alias{mnorm}
\title{Euclidean Norm of a Matrix}
\usage{
  mnorm(x)
}
\arguments{
  \item{x}{a numeric matrix}
}
\value{
  norm of x
}
\description{
  Computes the euclidean (aka Frobenius) norm of a matrix
}
\examples{
M1 = matrix(1:9, 3, 3)

# norm of M1
mnorm(M1)

# one-dim (one row) matrix
M2 = matrix(1:15, 3, 5)

# norm of row1
mnorm(M2)
}
\seealso{
  \code{\link{vnorm}}
}

