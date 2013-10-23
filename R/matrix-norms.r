#' @title Matrix Norms
#' 
#' @description
#' Use \code{one_norm} to get the \code{p=1} norm of a matrix \cr
#' Use \code{max_norm} to get the maximum norm of a matrix \cr
#' Use \code{inf_norm} to get the infinit norm of a matrix
#' 
#' @details
#' one norm is the maximum absolute column sum of a matrix \cr
#' max norm is the elementwise norm \cr
#' infinit norm is the maximum absolute row sum of a matrix 
#' 
#' @param x a numeric matrix
#' @return the calculated norm
#' @export one_norm max_norm inf_norm
#' @aliases one_norm max_norm inf_norm
#' @examples
#' # matrix
#' M = matrix(1:9, 3, 3)
#' 
#' # one norm
#' one_norm(M)
#' 
#' # max norm
#' max_norm(M)
#' 
#' # inf norm
#' inf_norm(M)
one_norm <- function(x)
{
  if (!is.numeric(x)) 
    stop("'one_norm()' requires a numeric matrix (or vector)")
  
  if (is_vector(x)) {
    return(sum(abs(x)))
  }
  if (is_matrix(x)) {
    # absolute column sum
    abs_col_sum = apply(abs(x), 2, sum)
    return(max(abs_col_sum))    
  }
}

max_norm <- function(x)
{
  if (!is.numeric(x)) 
    stop("'max_norm()' requires a numeric matrix (or vector)")
  
  # output
  max(abs(x))
}

inf_norm <- function(x)
{
  if (!is.numeric(x)) 
    stop("'inf_norm()' requires a numeric matrix (or vector)")
  
  if (is_vector(x)) {
    return(sum(abs(x)))
  }
  if (is_matrix(x)) {
    # absolute row sum
    abs_row_sum = apply(abs(x), 1, sum)
    return(max(abs_row_sum)) 
  }
}
