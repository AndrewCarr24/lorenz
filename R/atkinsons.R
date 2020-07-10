# Computes Atkinson's Coefficient / Takes income vector, eta parameter / Returns Atkinson coefficient
atkinson_func <- function(x, eta = 0){

  # Making values <= 0 equal 1
  x[x <= 0] <- 1

  N <- length(x)
  xbar <- sum(x)/N

  if(eta < 1){
    return(1 - 1/xbar*(sum(1/N*x^(1-eta)))^(1/(1-eta)))
  }

}
