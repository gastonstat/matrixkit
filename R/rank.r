#' @title Rank of matrix
#' 
#' @description returns the rank of a square matrix 
#' 
#' @details The rank is calculated based on the selected method
#' @param x square numeric matrix
#' @param method type of decomposition: \code{"qr"} or \code{"chol"}
#' @export
#' @examples
#' # a square matrix
#' set.seed(5)
#' M1 = matrix(runif(9), 3, 3)
#' 
#' # rank
#' rank_matrix(M1)
#' 
#' # another matrix
#' M2 = cbind(c(1,1), c(-2,-2))
#' 
#' # rank
#' rank_matrix(M2)
rank_matrix <- function (x, method = "qr")
{
  if (!is_square_matrix(x))
    stop("\n'rank_matrix()' requires a square matrix" )
  
  if (method == "qr") {
    rank = qr(x)$rank 
  } else {
    rank = attr(chol(x, pivot = TRUE), "rank") 
  }
  # output
  rank
}
