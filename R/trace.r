#' @title Trace of a square matrix
#' @description calculates the trace of a square matrix
#' @param x numeric square matrix
#' @export trace_matrix tr
#' @aliases trace_matrix tr
#' @examples
#' # a square matrix
#' set.seed(5)
#' M = matrix(runif(9), 3, 3)
#' 
#' # trace
#' trace_matrix(M)
#' 
#' # synonym function
#' tr(M)
trace_matrix <- function(x)
{
  if (is_not_square_numeric_matrix(x))
    stop("\n'trace_matrix()' requires a square numeric matrix")
  
  # output
  sum(diag(x))
}

tr <- function(x) {
  trace_matrix(x)
}
