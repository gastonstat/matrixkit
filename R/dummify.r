#' @title Dummify
#' 
#' @description Convert an object into a dummy matrix
#' 
#' @param x a vector, factor, matrix or data frame
#' @param labels should labels for row and columns be included? 
#' @param full whether all categories are included. When \code{full = FALSE}
#' then the first category is dropped out
#' @export
#' @examples
#' # vector
#' v = rep(c(1,2,3), 4)
#' dummify(v)
#' dummify(v, full = FALSE)
#' 
#' # factor
#' dummify(as.factor(v))
#' 
#' # matrix
#' M = cbind(c(1,2), c(3,4))
#' dummify(M)
#' 
#' # data frame
#' D = data.frame(b1=factor(c('a','b','a','b')), b2=factor(1:4))
#' dummify(D)
dummify <- function(x, labels, full) {
  UseMethod("dummify", x)
}

#' @S3method dummify default
dummify.default <- function(x, labels = TRUE, full = TRUE) {
  if (has_missing(x))
    stop("\nNo missing values allowed for 'dummify()'")    
}

#' @S3method dummify numeric
dummify.numeric <- function(x, labels = TRUE, full = TRUE) {
  dummify_vector(x, labels = labels, full = full)
}

#' @S3method dummify character
dummify.character <- function(x, labels = TRUE, full = TRUE) {
  dummify_vector(x, labels = labels, full = full)
}

#' @S3method dummify factor
dummify.factor <- function(x, labels = TRUE, full = TRUE) {
  dummify_factor(x, labels = labels, full = full)
}

#' @S3method dummify matrix
dummify.matrix <- function(x, labels = TRUE, full = TRUE) {
  dummify_matrix(x, labels = labels, full = full)
}

#' @S3method dummify data.frame
dummify.data.frame <- function(x, labels = TRUE, full = TRUE)
{
  # only factors and no missing values
  column_class = sapply(x, class)
  if (any(column_class != "factor"))
    stop("\n'dummify()' requires a data frame with factors")

  dummify_dataframe(x, labels = labels, full = full)
}




#' @title Vector to Dummy Matrix
#' 
#' @description Converts a vector into a dummy matrix
#' 
#' @param x vector
#' @param labels should labels for row and columns be included? 
#' @param full whether all categories are included. When \code{full = FALSE}
#' then the first category is removed.
#' @keywords internal
#' @export
dummify_vector <- function(x, labels = TRUE, full = TRUE)
{
  # don't convert vector to factor (or it will be slower)
  if (full) categs = unique(x) else categs = unique(x)[-1L]
  num_categs = length(categs) 
  
  Dummy = matrix(0, length(x), num_categs)
  for (j in 1:num_categs) {
    Dummy[x == categs[j],j] = 1
  }
  
  # whether to include row and column labels?
  if (labels) 
  {
    colnames(Dummy) = categs
    if (has_names(x)) {
      rownames(Dummy) = names(x)      
    } else {
      rownames(Dummy) = 1:length(x)      
    }
  }
  # output
  Dummy
}


#' @title Factor to Dummy Matrix
#' 
#' @description Converts a factor into a dummy matrix
#' 
#' @param x factor
#' @param labels should labels for row and columns be included? 
#' @param full whether all categories are included. When \code{full = FALSE}
#' then the first category is removed.
#' @keywords internal
#' @export
dummify_factor <- function(x, labels = TRUE, full = TRUE)
{
  if (full) categs = levels(x) else categs = levels(x)[-1L]
  num_categs = length(categs)
  
  Dummy = matrix(0, length(x), num_categs)
  for (j in 1:num_categs) {
    Dummy[x == categs[j],j] = 1
  }
  
  # whether to include row and column labels?
  if (labels) 
  {
    colnames(Dummy) = categs
    if (has_names(x)) {
      rownames(Dummy) = names(x)      
    } else {
      rownames(Dummy) = 1:length(x)      
    }
  }
  # output
  Dummy
}



#' @title Matrix to Dummy Matrix
#' 
#' @description Converts a matrix into a dummy matrix
#' 
#' @param x matrix
#' @param labels should labels for row and columns be included? 
#' @param full whether all categories are included. When \code{full = FALSE}
#' then the first category is removed.
#' @keywords internal
#' @export
dummify_matrix <- function(x, labels = TRUE, full = TRUE) 
{
  # 'categs_per_var' can be a matrix or a list
  categs_per_var = apply(x, 2, unique)
  
  if (is.list(categs_per_var))
  {
    return(dummy_matrix_list(x, categs_per_var, labels, full))
  } else {
    return(dummy_matrix_matrix(x, categs_per_var, labels, full))
  }
}


# when categories are in list form
dummy_matrix_list <- function(x, categs_per_var, labels, full) 
{
  num_obs = nrow(x)
  num_variables = ncol(x)
  if (!full) {
    categs_per_var = lapply(categs_per_var, function(y) y[-1L])
  }

  num_categs_per_var = lengths(categs_per_var)
  total_categs = sum(num_categs_per_var)
  var_index = indexify(categs_per_var)
  
  Dummy = matrix(0, num_obs, total_categs)
  for (j in 1:num_variables)
  {
    aux_categs = categs_per_var[[j]]
    aux_matrix = matrix(0, num_obs, length(aux_categs))
    for (k in 1:length(aux_categs))
    {
      tmp <- x[,j] == aux_categs[k]
      aux_matrix[tmp,k] = 1
    }
    Dummy[,var_index == j] = aux_matrix
  }    

  # whether to include row and column labels?
  if (labels) 
  {
    colnames(Dummy) = unlist(categs_per_var)
    if (has_names(x)) {
      rownames(Dummy) = rownames(x)      
    } else {
      rownames(Dummy) = 1:nrow(x)      
    }
  }
  # output
  Dummy
}


# when categories are in matrix form
dummy_matrix_matrix <- function(x, categs_per_var, labels, full) 
{
  num_obs = nrow(x)
  num_variables = ncol(x)
  if (!full) {
    categs_per_var = categs_per_var[-1L,]
  }
  
  num_categs_per_var = rep(nrow(categs_per_var), num_variables)
  total_categs = prod(dim(categs_per_var))
  var_index = indexify(num_categs_per_var)
  
  Dummy = matrix(0, num_obs, total_categs)
  for (j in 1:num_variables)
  {
    aux_categs = categs_per_var[,j]
    aux_matrix = matrix(0, num_obs, length(aux_categs))
    for (k in 1:length(aux_categs))
    {
      tmp <- x[,j] == aux_categs[k]
      aux_matrix[tmp,k] = 1
    }
    Dummy[,var_index == j] = aux_matrix
  }    

  # whether to include row and column labels?
  if (labels) 
  {
    if (has_colnames(x)) {
      col_names = rep(colnames(x), num_categs_per_var)
    } else {
      col_names = rep(paste("X", 1L:ncol(x), sep=""), num_categs_per_var)
    }
    colnames(Dummy) = paste(col_names, as.vector(categs_per_var), sep=".")
    if (has_rownames(x)) {
      rownames(Dummy) = rownames(x)      
    } else {
      rownames(Dummy) = 1:nrow(x)      
    }
  }
  # output
  Dummy
}


# combining dummify_vector and alply (from package 'plyr')
# (not as fast as expected)
# B = cbind(c(1,2,1,2), 1:4)
# dumatrix(B)
# dumatrix <- function(x) 
# {
#  dummies = alply(x, 2, dummify_vector)
#  do.call("cbind", dummies)
# }


#' @title Data Frame to Dummy Matrix
#' 
#' @description Converts a data frame of factors into a dummy matrix
#' (aka Complete Disjunctive Table)
#' 
#' @param x data frame with categorical variables as factors
#' @param labels should labels for row and columns be included? 
#' @param full whether all categories are included. When \code{full = FALSE}
#' then the first category is removed.
#' @keywords internal
#' @export
dummify_dataframe <- function(x, labels = TRUE, full = TRUE)
{  
  num_obs = nrow(x)
  num_vars = ncol(x)

  # number of categories per variable
  cats_per_var = lapply(x, levels)
  if (!full) {
    cats_per_var = lapply(cats_per_var, function(y) y[-1L])
  }
  num_cats_per_var = lengths(cats_per_var)
  cats_per_var = unlist(cats_per_var)
  
  # total number of categories
  num_cats = sum(num_cats_per_var)
  
  # build super-indicator matrix 
  Dummy = matrix(0, num_obs, num_cats)
  ini = cumsum(num_cats_per_var) - num_cats_per_var + 1
  fin = cumsum(num_cats_per_var)
  for (j in 1L:num_vars)
  {
    aux_levels = levels(x[,j])
    aux_matrix = matrix(0, num_obs, num_cats_per_var[j])
    for (k in 1L:num_cats_per_var[j])
    {
      tmp <- x[,j] == aux_levels[k]
      aux_matrix[tmp,k] = 1
    }
    Dummy[,ini[j]:fin[j]] = aux_matrix
  }

  # whether to include row and column labels?
  if (labels) 
  {   
    if (has_colnames(x)) {
      col_names = rep(colnames(x), num_cats_per_var)
    } else {
      col_names = rep(paste("X", 1L:ncol(x), sep=""), num_cats_per_var)
    }
    #colnames(Dummy) = cats_per_var
    colnames(Dummy) = paste(col_names, as.vector(cats_per_var), sep=".")
    if (has_rownames(x)) {
      rownames(Dummy) = rownames(x)      
    } else {
      rownames(Dummy) = 1L:nrow(x)      
    }
  }
  # output
  Dummy
}
