#' @title Lower Triangular
#' 
#' @description Extracts the lower triangular part of a matrix
#' 
#' @param x a matrix
#' @param diag whether the diagonal should be included
#' @export
#' @seealso \code{\link{upper_triangular}}
#' @examples
#' m = matrix(1:20, 4, 4)
#' 
#' # lower triangular part (without diagonal)
#' lower_triangular(m)
#' 
#' # lower triangular part (including diagonal)
#' lower_triangular(m, diag = TRUE)
lower_triangular <- function(x, diag = FALSE)
{
  if (is_not_square_matrix(x))
    stop("\n'lower_triangular()' requires a square matrix")
  
  if (diag) {
    x[upper.tri(x, diag = FALSE)] <- 0
  } else {
    x[upper.tri(x, diag = TRUE)] <- 0
  }
  # output
  x
}


#' @title Upper Triangular
#' 
#' @description Extracts the upper triangular part of a matrix
#' 
#' @param x a matrix
#' @param diag whether the diagonal should be included
#' @export
#' @seealso \code{\link{lower_triangular}}
#' @examples
#' m = matrix(1:20, 4, 4)
#' 
#' # upper triangular part (without diagonal)
#' upper_triangular(m)
#' 
#' # upper triangular part (including diagonal)
#' upper_triangular(m, diag = TRUE)
upper_triangular <- function(x, diag = FALSE)
{
  if (is_not_square_matrix(x))
    stop("\n'lower_triangular()' requires a square matrix")
  
  if (diag) {
    x[lower.tri(x, diag = FALSE)] <- 0
  } else {
    x[lower.tri(x, diag = TRUE)] <- 0
  }
  # output
  x
}
