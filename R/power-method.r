#' @title Power Method
#' 
#' @description Finds a dominant eigenvalue and its corresponding 
#' eigenvector of a square matrix by applying the Power Method with scaling
#' 
#' @param X a numeric matrix
#' @param v optional starting vector
#' @param eps convergence threshold
#' @param maxiter maximum number of iterations
#' @return a list containing the eigenvector, eigenvalue, and iterations
#' @export
#' @examples
#' A = cbind(c(2, 1), c(-12, -5))
#' power_method(A, v = rep(1, nrow(A)))
#' 
#' B = cbind(c(1, -2, 1), c(2, 1, 3), c(0, 2, 1))
#' power_method(B)
power_method <- function(X, v = NULL, eps = 1e-6, maxiter = 100)
{
  if (is_not_square_numeric_matrix(X))
    stop("\n'power_method()' requires a square numeric matrix")
  
  if (!is.null(v))
  {
    if (!is_numeric_vector(v))
      stop("\n'power_method()' requires 'v' to be a numeric vector")
    if (nrow(X) != length(v))
      stop("\n'X' is incompatible with 'v' in 'power_method()'")
  } else {
    v = rep(1, nrow(X))
  }
  
  if (!is_positive_decimal(eps))
    eps = 1e-6
  
  v_old = v
  steps = 1
  repeat 
  {
    v_new = X %*% v_old
    v_new = v_new / vnorm(v_new)
    if (vnorm(abs(v_new) - abs(v_old)) <= eps) break
    v_old = v_new
    steps = steps + 1
    if (steps == maxiter) break
  }
  # Rayleigh quotient
  lambda = sum((X %*% v_new) * v_new)
  # output
  list(vector = v_new, value = lambda, iter = steps)
}
