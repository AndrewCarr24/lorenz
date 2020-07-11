### Takes income distribution information and returns theil estimate from MCIB
mcib_to_theil <- function(freqs, means, bounds, N, G){

  theils_lst <- list()

  for(i in 1:15){

    bin_idx <- i

    lb <- bounds[bin_idx] + 1
    ub <- bounds[bin_idx+1]
    freq <- freqs[bin_idx]
    mean <- means[bin_idx]
    rel_freq <- freq/(ub - lb)
    midpoint <- (lb+ub)/2

    parms <- means_to_parms(freq, lb, ub, mean)

    theils_lst[[i]] <- parms_to_theil(parms[1]/N, parms[2]/N, lb, ub, G)

  }

  closed_theil_sum <- theils_lst %>% unlist %>% sum

  ### Upper bin -
  mean_top <- means[length(means)]
  lb <- 200000
  alpha <- mean_top/(mean_top - lb)
  freq_top <- freqs[length(freqs)]

  first <- ((alpha*lb^alpha)*freq_top)/(G*N*lb^(alpha-1)*(alpha-1)^2)
  second <- (alpha-1)*log(lb/G) + 1

  ## Theil est
  return((first*second) + closed_theil_sum)
}


# Returns parms of uniform or linear fitted line (not pdf)
means_to_parms <- function(freq, lb, ub, mean){

  midpoint <- (lb+ub)/2

  rel_freq <- freq/(ub - lb)
  ub <- (ub - lb)/sqrt(freq)
  mean <- (mean - lb)/sqrt(freq)
  rel_freq_scaled <- rel_freq/sqrt(freq)
  lb <- 0

  A = matrix(c((ub^3 - lb^3)/3, ((ub-lb)/2), (ub^2-lb^2)/2, 1), 2, 2)
  y = matrix(c(mean, rel_freq_scaled))

  parms <- solve(A, y)

  yint <- rel_freq - parms[1]*midpoint

  return(c(parms[1], yint))

}

# Getting theil contribution with integration by parts
parms_to_theil <- function(m, b, lb, ub, g){

  theil_integral <- function(x){
    return((1/g)*(log(x/g)*((m/3)*x^3 + (b/2)*x^2) - (m/9)*x^3 - (b/4)*x^2))
  }

  return(theil_integral(ub) - theil_integral(lb))

}
