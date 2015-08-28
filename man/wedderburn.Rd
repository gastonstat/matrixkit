\name{wedderburn}
\alias{rank_reduction}
\alias{wedderburn}
\title{Wedderburn Rank Reduction}
\usage{
  wedderburn(X, a = NULL, b = NULL)
}
\arguments{
  \item{X}{a numeric matrix}

  \item{a}{optional vector of length equal to the number of
  rows in \code{X}}

  \item{b}{optional vector of length equal to the number of
  columns in \code{X}}
}
\description{
  Rank reduction of a matrix by means of Wedderburn theorem
}
\details{
  when \code{a = NULL} and-or \code{b = NULL}, they are
  randomly generated using \code{rnorm()}
}
\examples{
set.seed(9)
X = matrix(runif(12), 4, 3)

# wedderburn's rank reduction
wedderburn(X)

# synonmym function
rank_reduction(X)
}

