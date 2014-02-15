\name{trace_matrix}
\alias{tr}
\alias{trace_matrix}
\title{Trace of a square matrix}
\usage{
  trace_matrix(x)
}
\arguments{
  \item{x}{square numeric matrix}
}
\description{
  calculates the trace of a square matrix
}
\examples{
# a square matrix
set.seed(5)
M = matrix(runif(9), 3, 3)

# trace
trace_matrix(M)

# synonym function
tr(M)
}

