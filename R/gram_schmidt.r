
gram_schmidt <- function(x, eps = 1e-15)
{
  m = ncol(x)
  Q = x
  R = matrix(0, m, m)
  k = 0
  
  for (i in 1L:m) 
  {
    if (i > 1L) 
    {
      for (j in 1L:(i - 1)) 
      {
        a = sum(Q[,i] * Q[,j])
        R[j,i] = a
        Q[,i] = Q[,i] - a * Q[,j]
      }
    }
    a = sqrt(sum(Q[,i]^2))
    if (a < eps) next()
    Q[,i] = Q[,i] / a
    R[i,i] = a
    k = k + 1
  }
  list(Q = Q, R = R, k = k)
}
