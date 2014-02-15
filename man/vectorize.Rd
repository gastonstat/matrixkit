\name{vectorize}
\alias{vec}
\alias{vectorize}
\title{Vectorize}
\usage{
  vectorize(x, as.column = TRUE)
}
\arguments{
  \item{x}{a numeric matrix (or data.frame)}

  \item{as.column}{whether to return results in a one
  column matrix}
}
\description{
  Returns a vector that is a stack of the columns of
  \code{x}
}
\examples{
x = matrix(1:12, 4, 3)
vectorize(x)

# synonym function
vec(x)

vectorize(x, as.column = FALSE)

vectorize(iris[1:5, 1:3])

vectorize(1:5)
}

