# Functions for new kind of interpolation

lorenz_to_coefs2 <- function(lorenz_df, bounds, G){

  slopes <- bounds/G
  coefs_lst <- list()

  for(idx in 1:(nrow(lorenz_df)-2)){

    start_slope <- slopes[idx]
    end_slope <- slopes[idx+1]

    coefs_lst[[idx]] <- solve(as.matrix(cbind(c(lorenz_df[idx:(idx+1),1]^3, 3*lorenz_df[idx:(idx+1),1]^2),
                                              c(lorenz_df[idx:(idx+1),1]^2, 2*lorenz_df[idx:(idx+1),1]),
                                              c(lorenz_df[idx:(idx+1),1], 1, 1),
                                              c(1, 1, 0, 0))),
                              as.matrix(c(lorenz_df[idx:(idx+1),2], start_slope, end_slope))) %>% unname

  }

  return(coefs_lst)

}


perc_to_slope2 <- function(inputs, lorenz_df, coefs, no_cubic = FALSE){

  x_nums <- lorenz_df$x

  sapply(inputs, function(x){

    idx <- which(x < x_nums)[1] - 1

    if(is.na(idx)){idx <- length(coefs)}

    return(3*coefs[[idx]][1]*x^2 + 2*coefs[[idx]][2]*x + coefs[[idx]][3])

  })

}
