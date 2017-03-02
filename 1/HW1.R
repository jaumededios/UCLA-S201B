ones  <- matrix( rep(1,100), ncol = 1)
rpart <- matrix( rnorm(200), ncol = 2)
X <- cbind(ones,rpart)
