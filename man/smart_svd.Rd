\name{smart_svd}
\alias{smart_svd}
\title{Smart Singular Value Decomposition (SVD)}
\usage{
  smart_svd(m, tol)
}
\arguments{
  \item{m}{a numeric matrix}

  \item{tol}{tolerance threshold for positiveness}
}
\description{
  Computes SVD in which only positive singular values are
  returned
}
\details{
  The signs of the columns vectors in \code{u} and \code{v}
  may be different from that given by \code{\link{svd}}.
}
\examples{
set.seed(321)
m = matrix(runif(15), 5, 3)

smart_svd(m)
}

