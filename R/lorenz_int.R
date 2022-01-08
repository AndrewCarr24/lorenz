#' Computes income inequality statistics derived with Lorenz interpolation.
#' @param freqs A vector of counts in income brackets.
#' @param bounds A vector of income bracket boundaries.
#' @param G Grand mean of income distribution.
#' @param stat (optional) Return income statistic instead of sample incomes.
#' #' @param slope_parm (default = .9) Slope parameter that influences the shape of the function fitted to the Lorenz curve.
#' @return Income inequality statistics derived with Lorenz interpolation.
#' @examples
#' ex_freqs <- c(45, 31, 33, 27, 43, 40, 51, 50, 63, 97, 121, 132, 64, 54, 32, 12)
#' ex_bounds <- c(0, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000, 50000, 60000, 75000,
#' 100000, 125000, 150000, 200000)
#' ex_mean <- 66500
#' lorenz_int(ex_freqs, ex_bounds, ex_mean)
#' @export
#' @importFrom magrittr %>%
# Lorenz interpolation function
lorenz_int <- function(freqs, bounds, G, stat = "gini", slope_parm = .9){

  ######################
  ## Helper functions ##
  ######################

  # Takes x coordinates of Lorenz curve / Returns coordinates that define spline function to approximate Lorenz curve
  lorenz_to_coefs <- function(lorenz_df, slope_parm, bounds, G){

    # Adjusts cubic applied to top bin based to ensure cubic does not violate conditions of a Lorenz curve
    cubic_adjust <- function(bin_idx, x_1, x_2, y_1, bounds, G){

      A <- matrix(c(x_1^2, 2*x_1, 2*x_2,
                    x_1,   1,     1,
                    1,     0,     0), 3, 3)
      y <- matrix(c(y_1, bounds[bin_idx]/G, bounds[bin_idx+1]/G), 3, 1)

      coefs <- solve(A, y)
      a <- coefs[1]
      b <- coefs[2]
      c <- coefs[3]
      bin_mid <- (x_1+x_2)/2
      mid_slope <- 2*a*bin_mid + b

      # Cubic
      A = matrix(c(x_1^3, 3*x_1^2, 3*x_2^2, 3*bin_mid^2,
                   x_1^2, 2*x_1,   2*x_2,   2*bin_mid,
                   x_1,   1,       1,       1,
                   1,     0,       0,       0), 4, 4)

      # Incrementally reducing slope at midpoint of top bin
      y <- matrix(c(y_1, bounds[bin_idx]/G, bounds[bin_idx+1]/G, mid_slope*.9999), 4, 1)

      coef_test <- solve(A, y)

      while(6*coef_test[1]*x_1 + 2*coef_test[2] > 0 & 6*coef_test[1]*x_2 + 2*coef_test[2] > 0){

        coef_orig <- coef_test
        mid_slope <- mid_slope*.9999
        y <- matrix(c(y_1, bounds[bin_idx]/G, bounds[bin_idx+1]/G, mid_slope), 4, 1)
        coef_test <- solve(A, y)

      }

      return(coef_orig)

    }

    coefs_lst <- list()
    for(i in 1:(nrow(lorenz_df)-2)){

      bin_idx <- i

      x_1 <- lorenz_df$x[bin_idx]
      x_2 <- lorenz_df$x[bin_idx+1]
      x_3 <- lorenz_df$x[bin_idx+2]

      A = matrix(c(x_1^3, 3*x_1^2, 3*x_2^2, 3*x_3^2,
                   x_1^2, 2*x_1,   2*x_2,   2*x_3,
                   x_1,   1,       1,       1,
                   1,     0,       0,       0), 4, 4)

      if(bin_idx == 1){first_y <- 0}else{first_y <- a*x_1^3 + b*x_1^2 + c*x_1 + d}
      if(bin_idx == 15){
        last_y <- 1
        A[4,] <- c(1, 1, 1, 1)
      }else{
        last_y <- bounds[bin_idx+2]/G
      }

      y = matrix(c(first_y, bounds[bin_idx]/G, bounds[bin_idx+1]/G, last_y), 4, 1)

      coefs <- solve(A, y)

      coefs_lst[[i]] <- coefs

      a <- coefs[1]
      b <- coefs[2]
      c <- coefs[3]
      d <- coefs[4]

      # Checking valid lorenz
      while(6*a*x_1 + 2*b < 0 | 6*a*x_2 + 2*b < 0){

        # print(paste("up ping", i))
        x_3 <- x_3*1.0001

        A = matrix(c(x_1^3, 3*x_1^2, 3*x_2^2, 3*x_3^2,
                     x_1^2, 2*x_1,   2*x_2,   2*x_3,
                     x_1,   1,       1,       1,
                     1,     0,       0,       0), 4, 4)

        if(bin_idx == 1){first_y <- 0}else{first_y <- coefs_lst[[bin_idx-1]][1]*x_1^3 + coefs_lst[[bin_idx-1]][2]*x_1^2 +
          coefs_lst[[bin_idx-1]][3]*x_1 + coefs_lst[[bin_idx-1]][4]}

        if(bin_idx == (nrow(lorenz_df)-2)){
          last_y <- last_y*.9999
          A[4,] <- c(1, 1, 1, 1)
        }else{
          last_y <- bounds[bin_idx+2]/G
        }

        y = matrix(c(first_y, bounds[bin_idx]/G, bounds[bin_idx+1]/G, last_y), 4, 1)

        coefs <- solve(A, y)

        coefs_lst[[i]] <- coefs

        a <- coefs[1]
        b <- coefs[2]
        c <- coefs[3]
        d <- coefs[4]

        if(bin_idx == (nrow(lorenz_df)-2)){

          coefs_lst[[i]] <- coefs <- cubic_adjust(bin_idx, x_1, x_2, first_y, bounds, G)
          a <- coefs[1]
          b <- coefs[2]
          c <- coefs[3]
          d <- coefs[4]

        }

      }

    }

    ##########

    # Fitting quadratic function with start slope to the top category
    x_1 = lorenz_df$x[nrow(lorenz_df)-1]
    x_2 = 1
    start_slope = freqs[length(freqs)]/G

    A = matrix(c(x_1^2, x_2^2, 2*x_1,
                 x_1, x_2, 1,
                 1, 1, 0), 3, 3)

    if(length(coefs_lst[[length(coefs_lst)]]) == 4){
      y_1 <- coefs_lst[[length(coefs_lst)]][1]*x_1^3 + coefs_lst[[length(coefs_lst)]][2]*x_1^2 + coefs_lst[[length(coefs_lst)]][3]*x_1 + coefs_lst[[length(coefs_lst)]][4]
    }else if(length(coefs_lst[[length(coefs_lst)]]) == 3){
      y_1 <- coefs_lst[[length(coefs_lst)]][1]*x_1^2 + coefs_lst[[length(coefs_lst)]][2]*x_1 + coefs_lst[[length(coefs_lst)]][3]
    }
    y_2 <- 1
    y = matrix(c(y_1, y_2, start_slope), 3, 1)
    quad_coefs <- solve(A, y)

    mid_x <- (lorenz_df$x[(length(lorenz_df$x)-1)] + lorenz_df$x[(length(lorenz_df$x))])/2
    slope_mid_x <- 2*quad_coefs[1]*mid_x + quad_coefs[2]

    # Cubic to two points and two slopes
    A = matrix(c(x_1^3, x_2^3, 3*x_1^2, 3*mid_x^2,
                 x_1^2, x_2^2, 2*x_1, 2*mid_x,
                 x_1, x_2, 1, 1,
                 1, 1, 0, 0), 4, 4)

    y = matrix(c(y_1,
                 y_2,
                 start_slope,
                 slope_parm*slope_mid_x), 4, 1)

    final_coefs <- solve(A, y)
    a <- final_coefs[1]; b <- final_coefs[2]
    while((6*a*x_1 + 2*b < 0 | 6*a*x_2 + 2*b < 0) & slope_parm < 1){

      slope_parm <- slope_parm*1.001
      y = matrix(c(y_1,
                   y_2,
                   start_slope,
                   slope_parm*slope_mid_x), 4, 1)
      final_coefs <- solve(A, y)
      a <- final_coefs[1]; b <- final_coefs[2]
    }

    coefs_lst[[length(coefs_lst) + 1]] <- final_coefs

    return(coefs_lst)

  }

  # Get incomes from each bin
  get_unweighted_incs <- function(lorenz_df, lorenz_coefs, num_cats, num_splits, G){

    purrr::map(1:num_cats, function(idx_main){

      if(length(lorenz_coefs[[idx_main]]) == 3){

        xs <- seq(lorenz_df$x[idx_main], lorenz_df$x[idx_main+1], (lorenz_df$x[idx_main+1]-lorenz_df$x[idx_main])/num_splits)
        ys <- lorenz_coefs[[idx_main]][1]*xs^2 + lorenz_coefs[[idx_main]][2]*xs + lorenz_coefs[[idx_main]][3]
        slopes <- purrr::map(1:num_splits, function(idx){ (ys[idx+1]-ys[idx])/(xs[idx+1]-xs[idx]) }) %>% unlist

      }else if(length(lorenz_coefs[[idx_main]]) == 4){

        xs <- seq(lorenz_df$x[idx_main], lorenz_df$x[idx_main+1], (lorenz_df$x[idx_main+1]-lorenz_df$x[idx_main])/num_splits)
        ys <- lorenz_coefs[[idx_main]][1]*xs^3 + lorenz_coefs[[idx_main]][2]*xs^2 + lorenz_coefs[[idx_main]][3]*xs + lorenz_coefs[[idx_main]][4]
        slopes <- purrr::map(1:num_splits, function(idx){ (ys[idx+1]-ys[idx])/(xs[idx+1]-xs[idx]) }) %>% unlist

      }

      return(slopes*G)

    })

  }

  # Compute Atkinson from incomes
  atkinson_wgt <- function(x, w, epsilon = .2, wscale = 1000, G){

    if(is.null(w))
      w <- rep(1/length(x), length(x))
    wr <- round(w*wscale, digits = 0)
    xw <- rep(x, wr)

    xw_mean <- mean(xw^(1-epsilon))
    A <- 1 - (1/G)*(xw_mean)^(1/(1-epsilon))

    return(A)

  }

  ## Compute income shares from incomes
  get_inc_shares <- function(inc_lst, wts, num_splits){

    wts_lng <- rep(wts, each = num_splits)
    incs_fin <- inc_lst %>% unlist

    incs <- rep(1, length(inc_lst %>% unlist)) * rep(wts,each=100)
    marker <- cumsum(incs)/sum(incs)
    quants <- purrr::
    
    
    
    
    
    (c(.2, .4, .6, .8, .95), ~(inc_lst %>% unlist)[which(marker > .x)[1]]) %>% unlist
    wts_tot <- sum(incs_fin * wts_lng)

    is_20 <- sum(incs_fin[incs_fin < quants[1]] * wts_lng[incs_fin < quants[1]])/wts_tot
    is_40 <- sum(incs_fin[incs_fin >= quants[1] & incs_fin < quants[2]] * wts_lng[incs_fin >= quants[1] & incs_fin < quants[2]])/wts_tot
    is_60 <- sum(incs_fin[incs_fin >= quants[2] & incs_fin < quants[3]] * wts_lng[incs_fin >= quants[2] & incs_fin < quants[3]])/wts_tot
    is_80 <- sum(incs_fin[incs_fin >= quants[3] & incs_fin < quants[4]] * wts_lng[incs_fin >= quants[3] & incs_fin < quants[4]])/wts_tot
    is_100 <- sum(incs_fin[incs_fin >= quants[4]] * wts_lng[incs_fin >= quants[4]])/wts_tot

    return( c(is_20, is_40, is_60, is_80, is_100) )

  }

  ##########################
  ## LORENZ INTERPOLATION ##
  ##########################

  # Get lorenz curve parameters (lorenz_df is x coordinates of LC which are known)
  lorenz_df <- tibble::tibble(x = c(0, cumsum(freqs)/sum(freqs))) %>% dplyr::distinct()

  # Getting LC parameters
  lorenz_coefs <- lorenz_to_coefs(lorenz_df = lorenz_df, slope_parm = slope_parm, G = G, bounds = bounds)

  # Interpolating LC to get weighted exact incomes
  num_splits <- 100
  inc_lst <- get_unweighted_incs(lorenz_df, lorenz_coefs, length(lorenz_coefs), num_splits, G)
  wts <- diff(lorenz_df$x)
  inc_lst <- purrr::map(inc_lst, function(incs){ purrr::map(incs, function(inc){if(inc<0){0}else{inc}}) %>% unlist})

  # Estimating income statistics from weighted exact incomes
  if(stat == "gini"){

    return(dineq::gini.wtd(inc_lst %>% unlist, rep(wts, each = num_splits)))

  }else if(stat == "theil"){

    return(dineq::theil.wtd(inc_lst %>% unlist, rep(wts, each = num_splits)))

  }else if(stat == "sd"){

    wts_lng <- rep(wts, each = num_splits)

    Hmisc::wtd.var(inc_lst %>% unlist, weights=wts_lng, method=c('unbiased', 'ML')) %>% sqrt %>% return

  }else if(stat == "atkinson"){

    wts_lng <- rep(wts, each = num_splits)

    atkinson_wgt(inc_lst %>% unlist, wts_lng, epsilon = .2, wscale = 1000, G = G)

  }else if(stat == "inc_share"){

    get_inc_shares(inc_lst, wts, num_splits)

  }else if(stat == "quantile"){

    incs <- rep(1, length(inc_lst %>% unlist)) * rep(wts,each=100)
    marker <- cumsum(incs)/sum(incs)
    purrr::map(c(.2, .4, .6, .8, .95), ~(inc_lst %>% unlist)[which(marker > .x)[1]]) %>% unlist

  }else if(stat == "bin_means"){

    purrr::map(inc_lst, ~mean(.x)) %>% unlist

  }

}




