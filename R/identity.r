#' @title idendity matrix
#' @description produces an indentity matrix of a given size
#' @param n positive integer indicating the size
#' @export
#' @examples
#' # 3x3 identity matrix
#' identity_matrix(3)
identity_matrix <- function(n) 
{
  if (!is_positive_integer(n))
    stop("\n'n' must be a positive integer")
  
  # output
  diag(1, n)
}
