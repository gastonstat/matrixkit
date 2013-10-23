\name{dummify}
\alias{dummify}
\title{Dummify}
\usage{
  dummify(x, labels, full)
}
\arguments{
  \item{x}{a vector, factor, matrix or data frame}

  \item{labels}{should labels for row and columns be
  included?}

  \item{full}{whether all categories are included. When
  \code{full = FALSE} then the first category is dropped
  out}
}
\description{
  Convert an object into a dummy matrix
}
\examples{
# vector
v = rep(c(1,2,3), 4)
dummify(v)
dummify(v, full = FALSE)

# factor
dummify(as.factor(v))

# matrix
M = cbind(c(1,2), c(3,4))
dummify(M)

# data frame
D = data.frame(b1=factor(c('a','b','a','b')), b2=factor(1:4))
dummify(D)
}

