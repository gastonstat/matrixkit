\name{VAV}
\alias{VAV}
\title{Scalar-Valued Variance}
\usage{
  VAV(X, center = TRUE)
}
\arguments{
  \item{X}{a numeric matrix}

  \item{center}{whether to center matrix \code{X}}
}
\value{
  The scalar-valued variance
}
\description{
  Scalar-Valued Variance of a matrix
}
\examples{
set.seed(33)
A = matrix(runif(12), 4, 3)

# get VAV
VAV(A)
}
\seealso{
  \code{\link{RV}}, \code{\link{COVV}}
}

