\name{COVV}
\alias{COVV}
\title{Scalar-Valued Covariance}
\usage{
  COVV(X, Y, center = TRUE)
}
\arguments{
  \item{X}{a numeric matrix}

  \item{Y}{a numeric matrix}

  \item{center}{whether to center matrices \code{X} and
  \code{Y}}
}
\value{
  The scalar-valued Covariance
}
\description{
  Scalar-Valued Covariance of two matrices
}
\examples{
set.seed(33)
A = matrix(runif(12), 4, 3)
B = matrix(rnorm(12), 4, 3)

# get COVV
COVV(A, B)
}
\seealso{
  \code{\link{RV}}, \code{\link{VAV}}
}

