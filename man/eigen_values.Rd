\name{eigen_values}
\alias{eigen_values}
\title{Eigen-values}
\usage{
  eigen_values(x, center = TRUE, scale = TRUE)
}
\arguments{
  \item{x}{a matrix or data frame}

  \item{center}{either a logical value or a numeric vector
  of length equal to the number of columns of \code{x} for
  centering}

  \item{scale}{either a logical value or a numeric vector
  of length equal to the number of columns of \code{x} for
  scaling}
}
\description{
  Compute eigenvalues of a matrix
}
\examples{
# using data USArrests
eigen_values(USArrests)
}
\seealso{
  \code{\link{singular_values}}, \code{\link{eigen}}
}

