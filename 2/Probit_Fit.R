# This corresponds to Exercise 10

#' Fit probit distribution with with response \code{response} and 
#' covariates \code{covariates}
#' 
#' IMPORTANT: As defined, the Probit_Fit function does not add a vector
#' of constants to the linear model, it must be added explicitly!
#' 
#' @param response Vector (column) containing the responses to fit the model
#' @param covariate Matrix containing the covariates to fit the model.
#' @param inverse_link optional. Inverse link to use. Defaults to pnorm 
#'        (probit)
#'         
#' @return c(coeff_estim,l,cov_estim) where coeff_estim is the estimated
#'         coefficient, l is the log-likelihood for the coefficient and 
#'         cov_estim is the (numerical) estimate of the variance.
#' 
#' 

function (response, covariate, inverse_link = pnorm){
  #Define the log-likelihood
  # coefficients are the coefficients to estimate
  # data is a list that contains: 
  # c(response, covariate, inverse_link)
  
  log_likelihood <- function(coefficient, response = 0, covariate = 0, inverse_link = 0){
    p <- inverse_link(covariate %*% coefficient);
    l <- response*log(p)+(1-response)*log(1-p);
    return(sum(l));
  }
  # Random initialization for the coefficints 
  # This could be improved by first normalizing all the
  # covariates, to ensure that all weights are initially
  # equal
  
  coefficient <- rnorm(dim(covariate)[2])*0;
  data <- c(response,covariate,inverse_link)
  result <- optim(coefficient, log_likelihood, control = list(fnscale = -1),
                  response= response, covariate=covariate, inverse_link = inverse_link,
                  hessian = TRUE)
  
  return (list("value" = result$value,
               "coef"  = result$par ,
               "var"   =-solve(result$hessian)))
}