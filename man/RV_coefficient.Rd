\name{RV_coefficient}
\alias{RV}
\alias{RV_coefficient}
\title{RV Coefficient}
\usage{
  RV_coefficient(X, Y, center = TRUE)
}
\arguments{
  \item{X}{numeric matrix of dimension \code{I,J}}

  \item{Y}{numeric matrix of dimension \code{I,K}}

  \item{center}{whether to center matrices \code{X} and
  \code{Y}}
}
\value{
  RV coefficient
}
\description{
  The RV Coefficient is a measure of similarity between
  squared symmetric matrices
}
\examples{
set.seed(33)
A = matrix(runif(12), 4, 3)
B = matrix(rnorm(12), 4, 3)
RV_coefficient(A, B)

# synonym function
RV(A, B)
}
\seealso{
  \code{\link{COVV}}, \code{\link{VAV}}
}

