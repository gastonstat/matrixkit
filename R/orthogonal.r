#' @title Orthogonal Basis
#' 
#' @description Obtain an orthogonal basis for a given matrix.
#' @details This functions is based on the \code{Q} matrix from the
#' QR decomposition (see \code{\link{qr}})
#' 
#' @param x a numeric matrix
#' @export
#' @examples
#' A = matrix(rnorm(35), 7, 5)
#' orthogonal(A)
#' 
#' B = cbind(1:10, 11:20, 31:40)
#' orthogonal(B)
orthogonal <- function(x)
{
  if (!is.numeric(x))
    stop("\n'orthogonal()' requires a numeric argument")
  # Q matrix from QR decomposition
  qr.Q(qr(x))
}
