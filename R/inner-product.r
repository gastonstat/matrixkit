#' @title Inner Product
#' 
#' @description 
#' Computes the inner product of two vectors (or two matrices) with 
#' a specified metric
#' 
#' @details \code{x} and \code{y} must be of same length or dimension. \cr
#' If \code{metric = NULL} this means the identity metric. 
#' 
#' @param x numeric vector or matrix
#' @param y numeric vector or matrix
#' @param metric either \code{NULL}, a numeric vector, 
#' or a numeric square matrix
#' @export inner_prod vector_inner_prod matrix_inner_prod
#' @aliases inner_prod vector_inner_prod matrix_inner_prod
#' @examples
#' # two vectors
#' u = 1:10
#' v = seq(0, 1, length.out = 10)
#'
#' # identity metric 
#' inner_prod(u, v)
#' 
#' # metric as vector
#' inner_prod(u, v, metric = rep(1/length(u), length(u)))
#' 
#' # metric as matrix
#' inner_prod(u, v, metric = diag(1/5, length(v)))
#' 
#' # two matrices
#' A = matrix(1:12, 4, 3)
#' B = matrix(seq(0, 1, length.out = 12), 4, 3)
#' 
#' inner_prod(A, B)
#' inner_prod(A, B, rep(0.3, 4))
#' inner_prod(A, B, metric = diag(1/2, nrow(A)))
inner_prod <- function(x, y, metric = NULL) 
{
  if (!is.numeric(x) || !is.numeric(y))
    stop("\n'inner_prod()' requires numeric arguments")
  
  if (is_one_dim(x) && is_one_dim(y)) 
  {
    if (different_length(x, y))
      stop("\n'inner_prod()' requires arguments of same length")
    
    return(vector_inner_prod(x, y, metric))
  } else {
    if (is_multidim(x) && is_multidim(y)) {
      if (different_dim(x, y))
        stop("\n'inner_pord()' requires arguments of same dimensions")
      
      return(matrix_inner_prod(x, y, metric))
    } else {
      # different sizes of x and y
      stop("\nincompatible arguments for 'inner_prod()'")
    }
  }  
}


vector_inner_prod <- function(x, y, metric) 
{
  check_metric(metric)
  if (is.null(metric)) { return(sum(x * y)) } 
  
  if (is.vector(metric)) {
    if (length(metric) != length(x))
      stop("\nincorrect length of 'metric' for 'inner_prod()'")
    
    x = vectorize(x, as.column = FALSE)
    y = vectorize(y, as.column = FALSE)
    return(sum(x * metric * y))
  }
  
  if (is.matrix(metric)) {
    if (nrow(metric) != length(x))
      stop("\nincorrect dimension of 'metric' for 'inner_prod()'")
    
    return(as.numeric(t(x) %*% metric %*% y))
  }
}


matrix_inner_prod <- function(x, y, metric) 
{
  check_metric(metric)
  if (is.null(metric)) {
    x = vectorize(x, as.column = FALSE)
    y = vectorize(y, as.column = FALSE)
    return(sum(x * y))
  }
  
  if (is.vector(metric)) {
    if (length(metric) != nrow(x))
      stop("\nincorrect length of 'metric' for 'inner_prod()'")
    
#    metric = diag(metric, nrow(x))
    metric_vec = rep(metric, ncol(x))
    x = vectorize(x, as.column = FALSE)
    y = vectorize(y, as.column = FALSE)
    return(sum(x * metric_vec * y))
  }
  
  if (nrow(metric) != nrow(x))
    stop("\nincorrect dimensions of 'metric' for 'inner_prod()'")
  # output
  tr(t(x) %*% metric %*% y)
}


#' @title Check Metric for Inner Product
#' @description Make sure the provided metric for an inner product is ok
#' @details if \code{metric = NULL} this means the identity metric
#' @param metric either \code{NULL}, a numeric vector, 
#' or a numeric square matrix
#' @return Returns \code{TRUE} if the metric is ok, errors otherwise
#' @keywords internal
#' @export
#' @examples
#' check_metric(NULL)
#' check_metric(rep(1, 5))
#' check_metric(diag(1:5))
#' check_metric(matrix(1:25, 5, 5))
check_metric <- function(metric) 
{
  if (is.null(metric)) {return(TRUE)} 
  
  if (!is.numeric(metric))
    stop("\n'inner_prod()' requires a numeric metric")
  
  # metric as vector
  if (is.vector(metric)) 
  {
    if (has_nas(metric)) {
      stop("\nmissing values in 'metric' for 'inner_prod()'")
    } else {
      return(TRUE)      
    }
  }
  
  # metric as matrix
  if (!is_square_matrix(metric)) {    
    stop("\n'inner_prod()' requires a square matrix")
  } else {
    if (has_nas(metric)) {
      stop("\nmissing values in 'metric' for 'inner_prod()'")
    } else {
      return(TRUE)
    }
  }
}
