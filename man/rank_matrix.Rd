\name{rank_matrix}
\alias{matrix_rank}
\alias{rank_matrix}
\title{Rank of matrix}
\usage{
  rank_matrix(x, method = "qr")
}
\arguments{
  \item{x}{numeric matrix}

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
set.seed(5)
M1 = matrix(runif(21), 3, 7)

# rank
rank_matrix(M1)

# another matrix
M2 = matrix(1:20, 4, 5)

# rank
rank_matrix(M2)
}

