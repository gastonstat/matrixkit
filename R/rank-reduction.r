#' @title Wedderburn Rank Reduction
#' 
#' @description Rank reduction of a matrix by means of Wedderburn theorem
#' 
#' @details when \code{a = NULL} and-or \code{b = NULL}, they are randomly
#' generated using \code{rnorm()}
#' @param X a numeric matrix
#' @param a optional vector of length equal to the number of rows in \code{X}
#' @param b optional vector of length equal to the number of columns in \code{X}
#' @export wedderburn rank_reduction
#' @aliases wedderburn rank_reduction
#' @examples
#' set.seed(9)
#' X = matrix(runif(12), 4, 3)
#' 
#' # wedderburn's rank reduction
#' wedderburn(X)
#' 
#' # synonmym function
#' rank_reduction(X)
wedderburn <- function(X, a = NULL, b = NULL) 
{
  if (!is_numeric_matrix(X))
    stop("\n'wedderburn()' requires a numeric matrix")
  if (is_one_dim(X))
    stop("\n'X' has rank 1 for 'wedderburn()'")
  
  if (!is.null(a)) {
    if (length(a) != nrow(X))
      stop("\n'a' and 'X' are incompatible for 'wedderburn()'")
  } else {
    a = rnorm(nrow(X))    
  }
  
  if (!is.null(b)) {
    if (length(b) != ncol(X))
      stop("\n'b' and 'X' are incompatible for 'wedderburn()'")
  } else {
    b = rnorm(ncol(X))    
  }
  
  alpha = as.numeric(t(a) %*% X %*% b)
  if (abs(alpha) < 1e-15)
    stop("\n'wedderburn()' failed to reduce rank of 'X'")
  
  # rank reduction (deflation)
  X - (1/alpha) * X %*% b %*% t(a) %*% X
}

rank_reduction <- function(X, a = NULL, b = NULL) {
  wedderburn(X, a = a, b = b) 
}
