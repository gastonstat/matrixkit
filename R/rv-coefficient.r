#' @title RV Coefficient
#' 
#' @description The RV Coefficient is a measure of similarity between 
#' squared symmetric matrices
#' 
#' @param X numeric matrix of dimension \code{I,J}
#' @param Y numeric matrix of dimension \code{I,K}
#' @param center whether to center matrices \code{X} and \code{Y}
#' @return RV coefficient
#' @seealso \code{\link{COVV}}, \code{\link{VAV}}
#' @export RV_coefficient RV
#' @aliases RV_coefficient RV
#' @examples
#' set.seed(33)
#' A = matrix(runif(12), 4, 3)
#' B = matrix(rnorm(12), 4, 3)
#' RV_coefficient(A, B)
#' 
#' # synonym function
#' RV(A, B)
RV_coefficient <- function(X, Y, center = TRUE)
{
  if (!is_numeric_matrix(X))
    stop("\n'RV()' requires 'X' to be a numeric matrix")
  if (!is_numeric_matrix(Y))
    stop("\n'RV()' requires 'Y' to be a numeric matrix")
  if (nrow(X) != nrow(Y))
    stop("\n'RV()' requires 'X' and 'Y' to have same number of rows")
  
  if (center) {
    X = scale(X, center=center, scale=FALSE)
    Y = scale(Y, center=center, scale=FALSE)
  }

  # outer products
  # 'tcrossprod()' slightly faster than 'X %*% t(X)'
  Sx = tcrossprod(X)
  Sy = tcrossprod(Y)

  # RV coefficient
  RV_numerator = tr(t(Sx) %*% Sy)
  RV_denom = sqrt(tr(Sx %*% Sx) * tr(Sy %*% Sy))
  # output
  RV_numerator / RV_denom
}

RV <- function(X, Y) {
  RV_coefficient(X, Y)
}

#' @title Scalar-Valued Variance 
#' @description Scalar-Valued Variance of a matrix
#' @param X a numeric matrix
#' @param center whether to center matrix \code{X}
#' @return The scalar-valued variance
#' @seealso \code{\link{RV}}, \code{\link{COVV}}
#' @export
#' @examples
#' set.seed(33)
#' A = matrix(runif(12), 4, 3)
#' 
#' # get VAV 
#' VAV(A)
VAV <- function(X, center = TRUE) 
{
  if (!is_numeric_matrix(X))
    stop("\n'VAV()' requires 'X' to be a numeric matrix")
  
  if (center)
    X = scale(X, center=center, scale=FALSE)
  # outer product
  # 'tcrossprod()' slightly faster than 'X %*% t(X)'
  XX = tcrossprod(X)
  # output
  tr(XX %*% XX)
}

#' @title Scalar-Valued Covariance 
#' @description Scalar-Valued Covariance of two matrices
#' @param X a numeric matrix
#' @param Y a numeric matrix
#' @param center whether to center matrices \code{X} and \code{Y}
#' @return The scalar-valued Covariance 
#' @export
#' @seealso \code{\link{RV}}, \code{\link{VAV}}
#' @examples
#' set.seed(33)
#' A = matrix(runif(12), 4, 3)
#' B = matrix(rnorm(12), 4, 3)
#' 
#' # get COVV 
#' COVV(A, B)
COVV <- function(X, Y, center = TRUE) 
{
  if (!is_numeric_matrix(X))
    stop("\n'COVV()' requires 'X' to be a numeric matrix")
  if (!is_numeric_matrix(Y))
    stop("\n'COVV()' requires 'Y' to be a numeric matrix")
  if (nrow(X) != nrow(Y))
    stop("\n'COVV()' requires 'X' and 'Y' to have same number of rows")
  
  if (center) {
    X = scale(X, center=center, scale=FALSE)
    Y = scale(Y, center=center, scale=FALSE)
  }

  # outer product
  # 'crossprod(X,Y)' slightly faster than 't(X) %*% Y'
  XY = crossprod(X, Y)
  YX = crossprod(Y, X)
  # output
  tr(XY %*% YX)
}
