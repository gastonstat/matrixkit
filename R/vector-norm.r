#' @title Vector Norm
#' 
#' @description Computes the (euclidean) norm of a vector
#' 
#' @param x a numeric vector (or a one-dimension matrix)
#' @return norm of x
#' @export
#' @examples
#' # vector
#' v = rep(1, 5)
#' 
#' # norm of v
#' vnorm(v)
#' 
#' # one-dim (one row) matrix
#' row1 = matrix(1:5, 1, 5)
#' 
#' # norm of row1
#' vnorm(row1)
#' 
#' # one-dim (one column) matrix
#' col1 = matrix(1:5, 5, 1)
#' 
#' # norm of col1
#' vnorm(col1)
vnorm <- function(x) {
  UseMethod("vnorm", x)
}

#' @S3method vnorm default
vnorm.default <- function(x) {
  if (!is.numeric(x))
    stop("\n'vnorm()' requires a numeric argument")
  # if x is matrix, check either 1 row or 1 column
  if (!is_one_dim(x))
    stop("\n'vnorm()' requires a 1-dimension object")
}

#' @S3method vnorm numeric
vnorm.numeric <- function(x) {
  sqrt(sum(x * x))
}

#' @S3method vnorm matrix
vnorm.matrix <- function(x) {
  x_dim = dim(x)
  if (x_dim[1L] > 1 && x_dim[2L] > 1)
    stop("\n'vnorm()' requires a 1-dimension object")
  
  # calculate norm
  if (x_dim[1L] == 1) return(sqrt(sum(x[1L,] * x[1L,])))
  if (x_dim[2L] == 1) return(sqrt(sum(x[,1L] * x[,1L])))
}
