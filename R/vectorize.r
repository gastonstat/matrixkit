#' @title Vectorize
#' 
#' @description 
#' Returns a vector that is a stack of the columns of \code{x}
#' 
#' @param x a numeric matrix (or data.frame)
#' @param as.column whether to return results in a one column matrix
#' @export vectorize vec
#' @aliases vectorize vec
#' @examples
#' x = matrix(1:12, 4, 3)
#' vectorize(x)
#' 
#' # synonym function
#' vec(x)
#' 
#' vectorize(x, as.column = FALSE)
#' 
#' vectorize(iris[1:5, 1:3])
#' 
#' vectorize(1:5)
vectorize <- function(x, as.column = TRUE) {
  UseMethod("vectorize", x)
}

# synonym function
vec <- function(x, as.column = TRUE) {
  vectorize(x, as.column = as.column)
}

#' @S3method vectorize default
vectorize.default <- function(x, as.column = TRUE) {
  if (!is.numeric(x))
    stop("\n'vectorize()' requires a numeric argument")
}

#' @S3method vectorize numeric
vectorize.numeric <- function(x, as.column = TRUE) {
  if (as.column) {
    return(t(t(x)))
  } else x
}

#' @S3method vectorize matrix
vectorize.matrix <- function(x, as.column = TRUE) {
  if (!is_numeric_matrix(x))
    stop("\n'vectorize()' requires a numeric matrix")
  
  vectorization(x, as.column = as.column)
}

#' @S3method vectorize data.frame
vectorize.data.frame <- function(x, as.column = TRUE) 
{
  if (!is_numeric_dataframe(x))
    stop("\n'vectorize()' requires a numeric data frame")
  
  x = as.matrix(x)
  vectorization(x, as.column = as.column)
}

# internal function for vectorization
vectorization <- function(x, as.column = FALSE) 
{
  if (as.column) {
    return( t(t(as.vector(x))) )
  } else {
    return(as.vector(x))
  }
}

