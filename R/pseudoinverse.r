#' @title matrix pseudo inverse
#' 
#' @description computes a pseudo-inverse of a matrix (Moore-Penrose inverse)
#' 
#' @param x a numeric matrix
#' @param tol tolerance threshold for singular value positiveness
#' @export
#' @examples
#' set.seed(3)
#' 
#' A = matrix(runif(9), 3, 3)
#' 
#' # pseudo inverse
#' B1 = pseudoinverse(A)
#' 
#' # A x B1
#' round(A %*% B1, 3)
#' 
#' # compare to 'ginv()'
#' require(MASS)
#' B2 = ginv(A)
#' B2
#' B1
pseudoinverse <- function(x, tol)
{
  SVD = smart_svd(x, tol)
  
  if (length(SVD$d) == 0) {
    return(array(0, dim(x)[2:1]))
  } else {
    return(SVD$v %*% (1/SVD$d * t(SVD$u)))
  }    
}
