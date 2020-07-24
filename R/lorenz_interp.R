#' Computes income inequality statistics derived with Lorenz interpolation.
#' @param freqs A vector of counts in income brackets.
#' @param bounds A vector of income bracket boundaries.
#' @param mean Grand mean of income distribution.
#' @param slope_parm (default = .9) Slope parameter that influences the shape of the function fitted to the Lorenz curve.
#' @param stat (optional) Return income statistic instead of sample incomes.
#' @param eta (optional) Parameter for Atkinson's coefficient.
#' @return Income inequality statistics derived with Lorenz interpolation.
#' @examples
#' ex_freqs <- c(45, 31, 33, 27, 43, 40, 51, 50, 63, 97, 121, 132, 64, 54, 32, 12)
#' ex_bounds <- c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 60000, 75000,
#' 100000, 125000, 150000, 200000)
#' ex_mean <- 66500
#' lorenz_interp(ex_freqs, ex_bounds, ex_mean)
#' @export
#' @importFrom magrittr %>%
lorenz_interp <- function(freqs, bounds, mean, slope_parm = .9, stat = "gini", eta = NA){

  if(stat == "atkinson" & is.na(eta)){
    stop("Atkinson coefficient requires an eta parameter.")
  }

  mcib_means <- freqs_to_mcib_means(freqs, bounds, mean)
  mcib_means[is.infinite(mcib_means)] <- 0
  mcib_means[is.nan(mcib_means)] <- 0

  # Lorenz curve
  lorenz_df <- as.data.frame(cbind(x = c(0, cumsum(freqs)/sum(freqs)), y = c(0, cumsum(freqs*mcib_means)/sum(freqs*mcib_means))))

  # Dealing with empty bins
  lorenz_df <- unique(lorenz_df)

  # Getting coefficients of fitted Lorenz curve
  lorenz_coefs <- lorenz_to_coefs(lorenz_df)
  # lorenz_coefs <- lorenz_to_coefs2(lorenz_df, bounds, mean)

  # Adding coefs for top cat
  slope_factor <- slope_parm
  lorenz_coefs[[(length(lorenz_coefs)+1)]] <- fit_poly_to_top(lorenz_df, lorenz_coefs, slope_factor)

  # Number of households
  N <- sum(freqs)


  # Sampling incomes from Lorenz curve
  interp_incomes <- perc_to_slope(seq(0, 1, 1/(N)), lorenz_df, lorenz_coefs)*mean
  interp_incomes[interp_incomes < 1] <- 1

  if(stat == "gini"){
    return(dineq::gini.wtd(interp_incomes))
  }else if(stat == "theil"){
    return(dineq::theil.wtd(interp_incomes))
  }else if(stat == "atkinson"){
    return(atkinson_func(interp_incomes, eta))
  }

}
