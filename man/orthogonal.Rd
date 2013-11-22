\name{orthogonal}
\alias{orthogonal}
\title{Orthogonal Basis}
\usage{
  orthogonal(x)
}
\arguments{
  \item{x}{a numeric matrix}
}
\description{
  Obtain an orthogonal basis for a given matrix.
}
\details{
  This functions is based on the \code{Q} matrix from the
  QR decomposition (see \code{\link{qr}})
}
\examples{
A = matrix(rnorm(35), 7, 5)
orthogonal(A)

B = cbind(1:10, 11:20, 31:40)
orthogonal(B)
}

