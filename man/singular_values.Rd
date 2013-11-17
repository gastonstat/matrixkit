\name{singular_values}
\alias{singular_values}
\title{Singular Values}
\usage{
  singular_values(x, center = TRUE, scale = TRUE)
}
\arguments{
  \item{x}{a matrix or data frame}

  \item{center}{either a logical value or a numeric vector
  of length equal to the number of columns of \code{x} for
  column centering (see \code{\link{scale}})}

  \item{scale}{either a logical value or a numeric vector
  of length equal to the number of columns of \code{x} for
  column scaling (see \code{\link{scale}})}
}
\description{
  Compute singular values of a matrix
}
\examples{
# using data USArrests
singular_values(USArrests)
}
\seealso{
  \code{\link{eigen_values}}, \code{\link{svd}}
}

