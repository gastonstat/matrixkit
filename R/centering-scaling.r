#' @title Weighted Average
#' @description Computes a weighted average of the elements in a vector
#' @param v numeric vector
#' @param weights optional numeric vector
#' @keywords internal
#' @export
weighted_average <- function(v, weights = NULL) 
{
  # output
  if (!is_numeric_vector(v))
    stop("\n'weighted_average()' requires a numeric vector")
  if (is.null(weights))
    weights = rep(1, length(v))

  sum(v * weights) / sum(weights)
}

#' @title Center Data
#' @description Returns centered data
#' @param data a numeric vector, matrix or data.frame
#' @param weights optional vector of weights
#' @export
#' @examples
#' # center vector
#' center_data(1:10)
#' 
#' # center matrix
#' m = matrix(1:9, 3, 3)
#' center_data(m)
#' 
#' # center data frame
#' center_data(iris[1:5,1:4])
center_data <- function(data, weights = NULL) 
{
  if (!is.null(weights)) {
    if (!is_numeric_vector(weights))
      stop("\n'center_data()' requires numeric 'weights'")    
  }
  
  # data as vector
  if (is_numeric_vector(data)) {
    if (is.null(weights)) { weights = rep(1, length(data)) }
    if (different_length(data, weights))
      stop("\n'data' and 'weights' have different length")
    centered = data - weighted_average(data, weights)
  } else {
    # data as matrix or data.frame
    if (is_numeric_tabular(data)) {
      if (is.null(weights)) { weights = rep(1, nrow(data)) }
      if (nrow(data) != length(weights))
        stop("\n'data' and 'weights' have different length")
      
      centers = apply(data, 2, weighted_average, weights)
      centered = sweep(data, 2, centers, FUN = "-")
    } else {
      stop("\n'data_center()' requires a numeric argument")
    }
  }
  # output
  centered
}


