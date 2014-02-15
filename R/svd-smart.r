#' @title Singular Value Decomposition (SVD) retaining positive values
#' 
#' @description Calculates SVD that retains only positive singular values
#' 
#' @param m a numeric matrix
#' @param tol tolerance threshold for positiveness of singular values
#' @export
#' @keywords internal
svd_positive <- function(m, tol)
{
  SVD = svd(m)
  
  if (missing(tol)) 
    tol = max(dim(m)) * max(SVD$d) * .Machine$double.eps
  
  positive_values = SVD$d > tol
  
  list(
    d = SVD$d[positive_values],
    u = SVD$u[, positive_values, drop = FALSE],
    v = SVD$v[, positive_values, drop = FALSE]
  )
}


#' @title Singular Value Decomposition (SVD) for a wide matrix
#' 
#' @description SVD of wide matrix (number of rows << number of columns)
#' 
#' @param m a numeric matrix
#' @param tol tolerance threshold for singular values
#' @export
#' @keywords internal
svd_wide = function(m, tol)
{
  B = m %*% t(m)
  SVD = svd(B, nv = 0)
  
  # rank of m
  if (missing(tol)) 
    tol = dim(B)[1L] * max(SVD$d) * .Machine$double.eps 
  
  # positive singular values 
  positive_values = SVD$d > tol                  
  d = sqrt(SVD$d[positive_values])
  
  # corresponding orthogonal basis vectors
  u = SVD$u[, positive_values, drop = FALSE]
  v = crossprod(m, u) %*% diag(1/d, nrow = length(d))   
  
  list(d = d, u = u, v = v)
}


#' @title Singular Value Decomposition (SVD) for a tall matrix
#' 
#' @description SVD of tall matrix (number of rows >> number of columns)
#' 
#' @param m a numeric matrix
#' @param tol tolerance threshold for singular values
#' @export
#' @keywords internal
svd_tall = function(m, tol)
{
  B = crossprod(m)   # pxp matrix
  SVD = svd(B, nu = 0)    # of which svd is easy..
  
  # determine rank of B  (= rank of m)
  if (missing(tol)) 
    tol = dim(B)[1L] * max(SVD$d) * .Machine$double.eps 
  
  # positive singular values of m  
  positive_values = SVD$d > tol                            
  d = sqrt(SVD$d[positive_values])
  
  # corresponding orthogonal basis vectors
  v = SVD$v[, positive_values, drop = FALSE]
  u = m %*% v %*% diag(1/d, nrow = length(d))
  
  list(d = d, u = u, v = v)
}



#' @title Smart Singular Value Decomposition (SVD)
#' 
#' @description 
#' Computes SVD in which only positive singular values are returned
#' 
#' @details
#' The signs of the columns vectors in \code{u} and \code{v} 
#' may be different from that given by \code{\link{svd}}.
#' 
#' @param m a numeric matrix
#' @param tol tolerance threshold for positiveness
#' @export
#' @examples
#' set.seed(321)
#' m = matrix(runif(15), 5, 3)
#' 
#' smart_svd(m)
smart_svd <- function(m, tol)
{  
  if (!is_numeric_matrix(m))
    stop("\n'smart_svd()' requires a numeric matrix")

  if (!missing(tol)) {
    if (!is_positive_decimal(tol))
      stop("\n'smart_svd()' requires 'tol' to be a positive decimal")    
  }
  
  num_rows = dim(m)[1L]
  num_cols = dim(m)[2L]

  if (num_rows > 2 * num_cols) {
    return(svd_tall(m, tol))
  }
  else if (2 * num_rows < num_cols)
  {  
    return(svd_wide(m, tol)) 
  }
  else # if num_cols and num_rows are approximately the same
  {
    return(svd_positive(m, tol))
  }
}

