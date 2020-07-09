#' Computes bracket means using mean-constrained integration over brackets.
#' @param freqs A vector of counts in income brackets.
#' @param agg Total income across brackets.
#' @param bounds A vector of income bracket boundaries.
#' @return MCIB means
#' @examples
#' bracket_frequencies <- c(45, 31, 33, 27, 43, 40, 51, 50, 63, 97, 121, 132, 64, 54, 32, 12)
#' bracket_boundaries <- c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000,
#'  60000, 75000, 100000, 125000, 150000, 200000)
#' aggregate_income <- sum(bracket_frequencies)*66500
#' freqs_to_mcib_means(bracket_frequencies, aggregate_income, bracket_boundaries)
#' @export
#' @importFrom magrittr %>%
freqs_to_mcib_means <- function(freqs, agg, bounds){

  N <- sum(freqs)
  F_x <- (cumsum(freqs)/sum(freqs))[1:(length(freqs)-1)]

  mcib_coords <- freq_and_bounds_to_mcib_coords(freqs[1:(length(freqs)-1)], bounds)
  slopes_ints <- mcib_coords_to_slopes_ints(mcib_coords)

  # Fixing lines that cross x-axis
  slopes_ints <-  x_adj(slopes_ints, bounds, mcib_coords)

  # Storing closed bracket means
  closed_bracket_means <- MCIB_closed_bracket_means(slopes_ints, bounds, freqs)

  # Storing top bracket mean estimate
  top_bracket_mean <- (agg - sum(freqs[1:(length(freqs)-1)]*closed_bracket_means, na.rm = T))/
    freqs[length(freqs)]

  return(c(closed_bracket_means, top_bracket_mean))

}


