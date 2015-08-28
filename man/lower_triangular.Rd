\name{lower_triangular}
\alias{lower_triangular}
\title{Lower Triangular}
\usage{
  lower_triangular(x, diag = FALSE)
}
\arguments{
  \item{x}{a matrix}

  \item{diag}{whether the diagonal should be included}
}
\description{
  Extracts the lower triangular part of a matrix
}
\examples{
m = matrix(1:20, 4, 4)

# lower triangular part (without diagonal)
lower_triangular(m)

# lower triangular part (including diagonal)
lower_triangular(m, diag = TRUE)
}
\seealso{
  \code{\link{upper_triangular}}
}

