#' Derives income inequality statitics using mean-constrained integration over brackets.
#' @param freqs A vector of counts in income brackets.
#' @param bounds A vector of income bracket boundaries.
#' @param mean Grand mean of income distribution.
#' @param stat (optional) Return income statistic instead of sample incomes.
#' @param eta (optional) Parameter for Atkinson's coefficient.
#' @return Income inequality statistics derived with mean-constrained integration over brackets.
#' @examples
#' ex_freqs <- c(45, 31, 33, 27, 43, 40, 51, 50, 63, 97, 121, 132, 64, 54, 32, 12)
#' ex_bounds <- c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000,
#'  60000, 75000, 100000, 125000, 150000, 200000)
#' ex_mean <- 66500
#' mcib(ex_freqs, ex_bounds, ex_mean)
#' @export
#' @importFrom magrittr %>%
#' @importFrom stats runif
mcib <- function(freqs, bounds, mean, stat = 'gini', eta = NA){

  N <- sum(freqs)
  agg <- mean*N
  rescaled_bounds <- bounds[2:length(bounds)]/sqrt(N)
  F_x <- (cumsum(freqs)/sum(freqs))[1:(length(freqs)-1)]

  mcib_coords <- freq_and_bounds_to_mcib_coords(freqs[1:(length(freqs)-1)], bounds)
  slopes_ints <- mcib_coords_to_slopes_ints(mcib_coords)
  slopes_ints_rescaled <- mcib_coords_to_slopes_ints(mcib_coords/sqrt(N))
  slopes_ints_rescaled <- x_adj(slopes_ints_rescaled, bounds/sqrt(N), mcib_coords/sqrt(N))

  # Polynomial coefficients of CDF brackets
  poly_coefs <- slopes_ints_rescaled_to_poly_coefs(slopes_ints_rescaled, rescaled_bounds, F_x)

  # Storing closed bracket means
  closed_bracket_means <- MCIB_closed_bracket_means(slopes_ints, bounds, freqs)

  # Storing top bracket mean estimate
  top_bracket_mean <- (agg - sum(freqs[1:(length(freqs)-1)]*closed_bracket_means))/
    freqs[length(freqs)]

  # Storing rescaled parms of pareto distribution for top bracket
  pareto_parms <- top_bracket_mean_to_rescaled_pareto_parms(top_bracket_mean, bounds, N)

  if(stat == "theil"){

    mcib_means <- freqs_to_mcib_means(freqs, bounds, mean)

    return(mcib_to_theil(freqs, mcib_means, bounds, N, mean))

  }else{

  # Using inverse-CDF method to take 10000 samples from MCIB-based pdf
  samples <- inverse_cdf_full(runif(10000), c(0, F_x, 1), poly_coefs, pareto_parms, F_x)*sqrt(N)

  if(stat == "gini"){
    return(dineq::gini.wtd(samples))
  }else if(stat == "atkinson"){
    return(atkinson_func(samples, eta))
  }

  }

}












