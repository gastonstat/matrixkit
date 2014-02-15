#' @title Orthogonal Projector (a.k.a. hat matrix)
#' 
#' @description produces an orthogonal projection matrix
#' i.e. X (X' X)-1 X'
#' 
#' @param x numeric matrix
#' @param inverse type of inverse. Options: \code{"solve"} (default)
#' and \code{"ginv"}
#' @export projector hat_matrix
#' @aliases projector hat_matrix
#' @examples
#' # a matrix
#' set.seed(5)
#' M = matrix(runif(15), 5, 3)
#' 
#' # projector
#' projector(M)
#' 
#' # synonym function
#' hat_matrix(M)
projector <- function(x, inverse = "solve") 
{
  if (!is_numeric_matrix(x))
    stop("\n'projector()' requires a numeric matrix")
  
  # output
  if (inverse == "solve") {
    proj = x %*% solve(t(x) %*% x) %*% t(x)    
  } else {
#    require(MASS)
    proj = x %*% ginv(t(x) %*% x) %*% t(x)  
  }
  proj
} 

hat_matrix <- function(x) {
  projector(x)
}
