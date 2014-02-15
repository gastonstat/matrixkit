\name{normalize}
\alias{normalize}
\title{Normalize}
\usage{
  normalize(x)
}
\arguments{
  \item{x}{numeric vector (or one-dimension matrix)}
}
\description{
  Normalizes a vector by its euclidean norm
}
\examples{
# vector
v = rep(1, 5)

# normalize of v
normalize(v)

# test it (norm = 1)
vnorm(normalize(v))

# matrix
m = matrix(rnorm(12), 4, 3)
apply(m, 2, normalize)
}
\seealso{
  \code{\link{vnorm}}
}

