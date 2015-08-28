\name{upper_triangular}
\alias{upper_triangular}
\title{Upper Triangular}
\usage{
  upper_triangular(x, diag = FALSE)
}
\arguments{
  \item{x}{a matrix}

  \item{diag}{whether the diagonal should be included}
}
\description{
  Extracts the upper triangular part of a matrix
}
\examples{
m = matrix(1:20, 4, 4)

# upper triangular part (without diagonal)
upper_triangular(m)

# upper triangular part (including diagonal)
upper_triangular(m, diag = TRUE)
}
\seealso{
  \code{\link{lower_triangular}}
}

