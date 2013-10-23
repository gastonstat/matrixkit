\name{rank_matrix}
\alias{rank_matrix}
\title{Rank of matrix}
\usage{
  rank_matrix(x, method = "qr")
}
\arguments{
  \item{x}{square numeric matrix}

  \item{method}{type of decomposition: \code{"qr"} or
  \code{"chol"}}
}
\description{
  returns the rank of a square matrix
}
\details{
  The rank is calculated based on the selected method
}
\examples{
# a square matrix
set.seed(5)
M1 = matrix(runif(9), 3, 3)

# rank
rank_matrix(M1)

# another matrix
M2 = cbind(c(1,1), c(-2,-2))

# rank
rank_matrix(M2)
}

