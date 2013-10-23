\name{one_norm}
\alias{inf_norm}
\alias{max_norm}
\alias{one_norm}
\title{Matrix Norms}
\usage{
  one_norm(x)
}
\arguments{
  \item{x}{a numeric matrix}
}
\value{
  the calculated norm
}
\description{
  Use \code{one_norm} to get the \code{p=1} norm of a
  matrix \cr Use \code{max_norm} to get the maximum norm of
  a matrix \cr Use \code{inf_norm} to get the infinit norm
  of a matrix
}
\details{
  one norm is the maximum absolute column sum of a matrix
  \cr max norm is the elementwise norm \cr infinit norm is
  the maximum absolute row sum of a matrix
}
\examples{
# matrix
M = matrix(1:9, 3, 3)

# one norm
one_norm(M)

# max norm
max_norm(M)

# inf norm
inf_norm(M)
}

