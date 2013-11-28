#' @title Rank of matrix
#' 
#' @description returns the rank of a square matrix 
#' 
#' @details The rank is calculated based on the selected method
#' @param x numeric matrix
#' @param method type of decomposition: \code{"qr"} or \code{"chol"}
#' @export rank_matrix matrix_rank
#' @aliases rank_matrix matrix_rank
#' @examples
#' set.seed(5)
#' M1 = matrix(runif(21), 3, 7)
#' 
#' # rank
#' rank_matrix(M1)
#' 
#' # another matrix
#' M2 = matrix(1:20, 4, 5)
#' 
#' # rank
#' rank_matrix(M2)
rank_matrix <- function (x, method = "qr")
{
  if (!is_numeric_matrix(x))
    stop("\n'rank_matrix()' requires a numeric matrix" )
  
  if (method == "qr") {
    rank = qr(x)$rank 
  } else {
    rank = attr(chol(x, pivot = TRUE), "rank") 
  }
  # output
  rank
}

matrix_rank <- function(x, method = "qr") {
  rank_matrix(x, method = method)
}
