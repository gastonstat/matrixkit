#' @title Eigen-values
#' 
#' @description Compute eigenvalues of a matrix
#' 
#' @param x a matrix or data frame
#' @param center either a logical value or a numeric vector of length equal 
#' to the number of columns of \code{x} for centering
#' @param scale either a logical value or a numeric vector of length equal 
#' to the number of columns of \code{x} for scaling
#' @seealso \code{\link{singular_values}}, \code{\link{eigen}}
#' @export
#' @examples
#' # using data USArrests
#' eigen_values(USArrests)
eigen_values <- function(x, center = TRUE, scale = TRUE)
{
  x = scale(x, center = center, scale = scale)
  singular_values = svd(x)$d
  (singular_values / sqrt(nrow(x) - 1))^2
}


#' @title Singular Values
#' 
#' @description Compute singular values of a matrix
#' 
#' @param x a matrix or data frame
#' @param center either a logical value or a numeric vector of length equal 
#' to the number of columns of \code{x} for column centering 
#' (see \code{\link{scale}})
#' @param scale either a logical value or a numeric vector of length equal 
#' to the number of columns of \code{x} for column scaling
#' (see \code{\link{scale}})
#' @seealso \code{\link{eigen_values}}, \code{\link{svd}}
#' @export
#' @examples
#' # using data USArrests
#' singular_values(USArrests)
singular_values <- function(x, center = TRUE, scale = TRUE)
{
  x = scale(x, center = center, scale = scale)
  svd(x)$d
}
