\name{vnorm}
\alias{vnorm}
\title{Vector Norm}
\usage{
  vnorm(x)
}
\arguments{
  \item{x}{a numeric vector (or a one-dimension matrix)}
}
\value{
  norm of x
}
\description{
  Computes the (euclidean) norm of a vector
}
\examples{
# vector
v = rep(1, 5)

# norm of v
vnorm(v)

# one-dim (one row) matrix
row1 = matrix(1:5, 1, 5)

# norm of row1
vnorm(row1)

# one-dim (one column) matrix
col1 = matrix(1:5, 5, 1)

# norm of col1
vnorm(col1)
}
\seealso{
  \code{\link{mnorm}}, \code{\link{normalize}}
}

