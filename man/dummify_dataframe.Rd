\name{dummify_dataframe}
\alias{dummify_dataframe}
\title{Data Frame to Dummy Matrix}
\usage{
  dummify_dataframe(x, labels = TRUE, full = TRUE)
}
\arguments{
  \item{x}{data frame with categorical variables as
  factors}

  \item{labels}{should labels for row and columns be
  included?}

  \item{full}{whether all categories are included. When
  \code{full = FALSE} then the first category is removed.}
}
\description{
  Converts a data frame of factors into a dummy matrix (aka
  Complete Disjunctive Table)
}
\keyword{internal}

