\name{check_metric}
\alias{check_metric}
\title{Check Metric for Inner Product}
\usage{
  check_metric(metric)
}
\arguments{
  \item{metric}{either \code{NULL}, a numeric vector, or a
  numeric square matrix}
}
\value{
  Returns \code{TRUE} if the metric is ok, errors otherwise
}
\description{
  Make sure the provided metric for an inner product is ok
}
\details{
  if \code{metric = NULL} this means the identity metric
}
\examples{
check_metric(NULL)
check_metric(rep(1, 5))
check_metric(diag(1:5))
check_metric(matrix(1:25, 5, 5))
}
\keyword{internal}

