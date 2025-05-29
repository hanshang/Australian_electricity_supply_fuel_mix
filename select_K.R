############################################
# way of selecting the number of components
############################################

select_K <- function(tau, eigenvalue)
{
  k_max = length(eigenvalue)
  k_all = rep(0, k_max-1)
  for(k in 1:(k_max-1))
  {
    k_all[k] = (eigenvalue[k+1]/eigenvalue[k])*ifelse(eigenvalue[k]/eigenvalue[1] > tau, 1, 0) + ifelse(eigenvalue[k]/eigenvalue[1] < tau, 1, 0)
  }
  K_hat = which.min(k_all)
  return(K_hat)
}
