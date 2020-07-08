# Auxiliary Functions for freqs_to_mcib_means #

# Takes numeric freq vec (just closed bracket freqs) and lower bound vec and returns mcib coords as tibble
freq_and_bounds_to_mcib_coords <- function(freqs, bounds){
  return(tibble::tibble(x = (bounds[1:(length(bounds)-1)]+bounds[2:length(bounds)])/2, y = freqs/diff(bounds)))
}


# Takes mcib coords tibble (midpoint and rel_freq) and returns slopes and ints in list
mcib_coords_to_slopes_ints <- function(points){

  points_lst <- split(points %>% as.matrix() %>% unname, seq(nrow(points))) %>% unname

  slopes <- purrr::map(seq_along(points_lst), function(idx){

    if(idx != length(points_lst)){
      (points_lst[[idx+1]][2]-points_lst[[idx]][2])/(points_lst[[idx+1]][1]-points_lst[[idx]][1])
    }else{
      (points_lst[[idx-1]][2]-points_lst[[idx]][2])/(points_lst[[idx-1]][1]-points_lst[[idx]][1])
    }
  })

  slopes <- purrr::map(seq_along(slopes), function(idx){
    if(idx != 1 & idx != length(slopes)){
      return((slopes[[idx-1]]+slopes[[idx]])/2)
    }else{
      return(slopes[[idx]])
    }
  }) %>% unlist


  slopes <- purrr::map(seq_along(slopes), function(idx){

    if(idx == 1){return(0)}else if(idx == length(slopes)){
      return(slopes[idx])
    }else{
      if(points_lst[[idx-1]][2] > points_lst[[idx]][2] & points_lst[[idx+1]][2] > points_lst[[idx]][2]){
        return(0)
      }else if(points_lst[[idx-1]][2] < points_lst[[idx]][2] & points_lst[[idx+1]][2] < points_lst[[idx]][2]){
        return(0)
      }else{
        return(slopes[idx])
      }
    }

  }) %>% unlist

  ints <- purrr::map2(slopes, points_lst, function(slope, coords){
    -slope*coords[1] + coords[2]
  }) %>% unlist

  return(list(slopes, ints))

}


# Takes lines that cross the x-axis and rotates them at the rel_freq point so that they remain positive over the bracket (new x-intercept is bracket boundary)
x_adj <- function(slopes_ints, bounds, mcib_coords){

  bad_idx <- which(purrr::pmap(list(slopes_ints[[1]], slopes_ints[[2]], seq_along(slopes_ints[[1]])), function(m, b, idx){

    LB <- bounds[idx]
    UB <- bounds[idx+1]

    x_int <- -b/m

    dplyr::between(x_int, LB, UB)
  }) %>% unlist)

  purrr::pmap(list(slopes_ints[[1]], slopes_ints[[2]], seq_along(slopes_ints[[1]])), function(m, b, idx){

    if(idx %in% bad_idx){

      if(m < 0){

        m_new <- (as.numeric(mcib_coords[idx, 2]) - 0)/(as.numeric(mcib_coords[idx, 1]) - bounds[idx+1])
        b_new <- (-1)*m_new*bounds[idx+1]
        return(c(m_new, b_new))

      }else if(m > 0){

        m_new <- (as.numeric(mcib_coords[idx, 2]) - 0)/(as.numeric(mcib_coords[idx, 1]) - bounds[idx])
        b_new <- (-1)*m_new*bounds[idx+1]
        return(c(m_new, b_new))

      }
    }else{

      return(c(m, b))

    }
  }) %>% do.call('rbind', .) %>% tibble::as_tibble(.name_repair = ~make.names(., unique = TRUE)) %>%
    purrr::map(., ~.x) %>% unname
}


# Takes slopes and ints and lower bounds and returns MCIB means of closed intervals
MCIB_closed_bracket_means <- function(slopes_ints, lower_bounds10, freqs){

  mean_integral <- function(m, b, x){
    return((m/3)*x^3 + (b/2)*x^2)}

  purrr::map(1:(length(lower_bounds10)-1), function(bracket_idx){

    LB <- (lower_bounds10)[bracket_idx]
    UB <- (lower_bounds10)[bracket_idx+1]

    m <- slopes_ints[[1]][bracket_idx]
    b <- slopes_ints[[2]][bracket_idx]

    mean_b <- mean_integral(m, b, UB) - mean_integral(m, b, LB)

    return(mean_b/freqs[bracket_idx])
  }) %>% unlist
}


# Auxiliary Functions for lorenz_interp #

# Takes tibble with Lorenz curve coordinates and return list of coefficients of functions fit to these coordinates
lorenz_to_coefs <- function(lorenz_df){

  coefs_lst <- list()

  for(idx in 1:(nrow(lorenz_df)-2)){

    if(idx != (nrow(lorenz_df)-2)){

      coefs_lst[[idx]] <- solve(as.matrix(cbind(lorenz_df[idx:(idx+2),1]^2, lorenz_df[idx:(idx+2),1], int = 1)),
                                as.matrix(lorenz_df[idx:(idx+2),2])) %>% unname

    }else{

      coefs_lst[[idx]] <- solve(as.matrix(cbind(lorenz_df[(idx-1):(idx+1),1]^2, lorenz_df[(idx-1):(idx+1),1], int = 1)),
                                as.matrix(lorenz_df[(idx-1):(idx+1),2])) %>% unname

    }

  }

  return(coefs_lst)

}


# Gets top category coefficients
fit_poly_to_top <- function(lorenz_df, lorenz_coefs, slope_factor){


  # Fitting quadratic function with start slope to the top category
  x_1 = lorenz_df$x[(length(lorenz_df$x)-1)]
  x_2 = lorenz_df$x[(length(lorenz_df$x))]
  start_slope = 2*lorenz_coefs[[nrow(lorenz_df)-2]][1]*x_1 + lorenz_coefs[[nrow(lorenz_df)-2]][2]

  A = matrix(c(x_1^2, x_2^2, 2*x_1,
               x_1, x_2, 1,
               1, 1, 0), 3, 3)

  y_1 <- lorenz_df$y[(length(lorenz_df$y)-1)]
  y_2 <- lorenz_df$y[(length(lorenz_df$y))]
  y = matrix(c(y_1, y_2, start_slope), 3, 1)
  quad_coefs <- solve(A, y)

  mid_x <- (lorenz_df$x[(length(lorenz_df$x)-1)] + lorenz_df$x[(length(lorenz_df$x))])/2
  slope_mid_x <- 2*quad_coefs[1]*mid_x + quad_coefs[2]

  # Cubic to two points and two slopes
  x_1 = lorenz_df$x[(length(lorenz_df$x)-1)]
  x_2 = lorenz_df$x[(length(lorenz_df$x))]

  A = matrix(c(x_1^3, x_2^3, 3*x_1^2, 3*mid_x^2,
               x_1^2, x_2^2, 2*x_1, 2*mid_x,
               x_1, x_2, 1, 1,
               1, 1, 0, 0), 4, 4)

  y = matrix(c(lorenz_df$y[(length(lorenz_df$y)-1)],
               lorenz_df$y[length(lorenz_df$y)],
               start_slope,
               slope_factor*slope_mid_x), 4, 1)

  return(solve(A, y))
}


# Samples from quantile function associated with given Lorenz curve function
perc_to_slope <- function(inputs, lorenz_df, coefs, no_cubic = FALSE){

  x_nums <- lorenz_df$x

  purrr::map(inputs, function(x){

    idx <- which(x < x_nums)[1] - 1

    if(is.na(idx)){idx <- length(coefs)}

    if(idx < length(coefs) | no_cubic == TRUE){

      return(2*coefs[[idx]][1]*x + coefs[[idx]][2])

    }else{

      return(3*coefs[[idx]][1]*x^2 + 2*coefs[[idx]][2]*x + coefs[[idx]][3])

    }

  }) %>% unlist

}
