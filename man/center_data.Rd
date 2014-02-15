\name{center_data}
\alias{center_data}
\title{Center Data}
\usage{
  center_data(data, weights = NULL)
}
\arguments{
  \item{data}{a numeric vector, matrix or data.frame}

  \item{weights}{optional vector of weights}
}
\description{
  Returns centered data
}
\examples{
# center vector
center_data(1:10)

# center matrix
m = matrix(1:9, 3, 3)
center_data(m)

# center data frame
center_data(iris[1:5,1:4])
}

