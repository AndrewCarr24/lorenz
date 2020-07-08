#' Derives samples from grouped income data using Lorenz Interpolation.
#' @param freqs_lst A vector of counts in income brackets.
#' @param bounds A vector of income bracket boundaries.
#' @param mean_lst Grand mean of income distribution.
#' @param slope_parm (default = .9) Slope parameter that influences the shape of the function fitted to the Lorenz curve.
#' @param stat (optional) Return income statistic instead of sample incomes.
#' @return A vector of exact incomes sampled from income distribution.
#' @examples
#' freqs_lst <- c(45, 31, 33, 27, 43, 40, 51, 50, 63, 97, 121, 132, 64, 54, 32, 12)
#' bounds <- c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 60000, 75000,
#' 100000, 125000, 150000, 200000)
#' mean_lst <- 66500
#' lorenz_interp(freqs_lst, bounds, mean_lst)
#' @export
#' @importFrom magrittr %>%
lorenz_interp <- function(freqs_lst, bounds, mean_lst, slope_parm = .9, stat = NA){

  if(!is.list(freqs_lst)){
    freqs_lst <- list(freqs_lst)
  }

  if(length(mean_lst) == 1){
    mean_lst <- list(mean_lst)
  }

  purrr::map2(freqs_lst, mean_lst, function(freqs, mean){

    agg <- sum(freqs)*mean

    mcib_means <- freqs_to_mcib_means(freqs, agg = agg, bounds = bounds)
    mcib_means[is.infinite(mcib_means)] <- 0

    # Lorenz curve
    lorenz_df <- tibble::tibble(x = c(0, cumsum(freqs)/sum(freqs)), y = c(0, cumsum(freqs*mcib_means)/sum(freqs*mcib_means)))

    # Dealing with empty closed bins
    lorenz_df <- lorenz_df %>% dplyr::filter(!duplicated(x,y))

    # Dealing with empty open bins
    if(sum(lorenz_df$x == 1) > 1){
      lorenz_df <- lorenz_df[1:(which(lorenz_df$x == 1)[1]),]
    }

    lorenz_coefs <- lorenz_to_coefs(lorenz_df)

    # Adding coefs for top cat
    slope_factor <- slope_parm
    lorenz_coefs[[(length(lorenz_coefs)+1)]] <- fit_poly_to_top(lorenz_df, lorenz_coefs, slope_factor)


    # Number of households
    N <- sum(freqs)

    interp_incomes <- perc_to_slope(seq(0, 1, 1/N), lorenz_df, lorenz_coefs)*mean

    interp_incomes[interp_incomes < 1] <- 1

    if(is.na(stat)){
      return(sort(interp_incomes))
    }else if(stat == "gini"){
      return(sort(interp_incomes))
    }

  }) %>% unlist

}
