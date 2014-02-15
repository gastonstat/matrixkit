#' @title vector and matrix of ones
#' 
#' @description 
#' \code{ones} produces either a vector or a matrix of ones \cr
#' \code{ones_vector} produces a vector of ones of a given length \cr
#' \code{ones_matrix} produces a matrix of ones of a given dimension
#' 
#' @param n indicates either the length (for vector) or
#' the number of rows (for matrix)
#' @param p indicates the number of columns (for matrix)
#' @aliases ones ones_vector ones_matrix
#' @export ones ones_vector ones_matrix
#' @seealso \code{\link{zeros}}
#' @examples
#' # vector of ones with 5 elements
#' ones(5)
#' # equivalently
#' ones_vector(5)
#' 
#' # 3x3 matrix of ones
#' ones(3, 3)
#' # equivalently
#' ones_matrix(3, 3)
#' 
#' # 1x3 matrix of ones
#' ones(1, 3)
#' 
#' # 3x1 matrix of ones
#' ones(3, 1)
ones <- function(n, p = NULL) 
{
  if (is.null(p)) {
    result = ones_vector(n)    
  } else {
    result = ones_matrix(n, p)
  }
  # output
  result
}

ones_vector <- function(n) 
{
  if (!is_positive_integer(n))
    stop("\n'ones()' must be a positive integer")
  
  # output
  rep(1, n)  
}

ones_matrix <- function(n, p) 
{
  if (!is_positive_integer(n) | !is_positive_integer(p))
    stop("\n'ones()'requires positive integers")
  
  # output
  matrix(1, nrow = n, ncol = p)  
}



#' @title vector and matrix of zeros
#' 
#' @description 
#' \code{zeros} produces either a vector or a matrix of zeros \cr
#' \code{zeros_vector} produces a vector of zeros of a given length \cr
#' \code{zeros_matrix} produces a matrix of zeros of a given dimension
#' 
#' @param n indicates either the length (for vector) or
#' the number of rows (for matrix)
#' @param p indicates the number of columns (for matrix)
#' @aliases zeros zeros_vector zeros_matrix
#' @export zeros zeros_vector zeros_matrix
#' @seealso \code{\link{ones}}
#' @examples
#' # vector of zeros with 5 elements
#' zeros(5)
#' # equivalently
#' zeros_vector(5)
#' 
#' # 3x3 matrix of zeros
#' zeros(3, 3)
#' # equivalently
#' zeros_matrix(3, 3)
#' 
#' # 1x3 matrix of zeros
#' zeros(1, 3)
#' 
#' # 3x1 matrix of zeros
#' zeros(3, 1)
zeros <- function(n, p = NULL) 
{
  if (is.null(p)) {
    result = zeros_vector(n)    
  } else {
    result = zeros_matrix(n, p)
  }
  # output
  result
}

zeros_vector <- function(n) 
{
  if (!is_positive_integer(n))
    stop("\n'zeros()' requires a positive integer")
  
  # output
  rep(0, n)  
}

zeros_matrix <- function(n, p) 
{
  if (!is_positive_integer(n) | !is_positive_integer(p))
    stop("\n'zeros()' requires positive integers")
  
  # output
  matrix(0, nrow = n, ncol = p)  
}
