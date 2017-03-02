library(ggplot2)

#IMPORTANT: ASSUMES Probit_Fit is on the current working directory!
# You can just copy and paste the source of the file Probit_Fit otherwise 
# instead of line 7

Probit_Fit <- dget("Probit_Fit.R")

n_exp = 200 #Number of data points
n_cov = 3 #Number of covariates

#Generate Covariates Randomly
covariates <- matrix(rnorm(n_exp*n_cov),ncol = n_cov, nrow = n_exp)

#Generate betas Randomly
real_beta <- matrix(c(.3,.2,.2))#matrix(rnorm(n_cov))

#Compute associated probabilities
probabilities = pnorm(covariates %*% real_beta)

#simulate a response
simulated_response= matrix(rbinom(n_exp,1, prob = probabilities))

#Fit the simulation
result <- Probit_Fit (simulated_response, covariates)

#Study a given coefficient (beta_i, i=coef)
#(since we chose them at random, 1 is as good as any other)
coef <- 1;

#Number of drawns (for bootstrap and direct draw)
times <- 100;

#Function that generates a whole new random drawn, and computes the result
drawn_helper <- function(probabilities, covariates){
  ne <- dim(covariates)[1]
  sr <- matrix(rbinom(ne,1, prob = probabilities))
  r <- Probit_Fit (sr, covariates)
  return(r$coef[1])
}

#Replicate the function above
Drawn <- replicate(times,
          drawn_helper(probabilities,covariates)
          )

#Same as above, now bootstraping instead
boot_helper <- function(covariates, responses){
  ne <- dim(covariates)[1]
  ind <- sample(1:ne,ne,TRUE)
  re <- responses[ind,]
  cov <- covariates[ind,]
  r <- Probit_Fit (re, cov)
  return(r$coef[1])
}

#Replicate the function above
Bootstrap <- replicate(times,
             boot_helper(covariates,simulated_response)
              )

#Function for the estimated covariance
Estimated <- function(x){
  return(dnorm(x,mean = result$coef[coef],sd = (result$var[coef,coef])**.5 ))
}

simulation = data.frame(Drawn, Bootstrap)

ggplot(simulation) + 
  geom_density(aes(Drawn,colour="Drawn"    )) +
  geom_density(aes(Bootstrap,colour="Bootstrap")) + 
  stat_function(aes(colour="Estimated"),fun = Estimated) +
  ggtitle("Comparison of the distributions") + xlab(expression(beta[1])) +ylab('Density')+
  labs(colour='Method') + theme(plot.title = element_text(hjust = 0.5))

