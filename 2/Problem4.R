library('sandwich')

# Exercise 15

n_exp = 1000

X = rnorm(n_exp)
Y = rnbinom(n_exp, size = 1/3., mu = exp(X/100))

Test = data.frame(X,Y)

fit <- glm(Y ~ X, data = Test, poisson(link = "log"))

print(vcov(fit))


# Exercise 16


# Yes, this is a copy of the code on the slides
bread=vcov(fit) 
score=estfun(fit)
meat=t(score)%*%score
sandwich=bread%*%meat%*%bread


print(sandwich)

#Exercise 17

replicate_helper <- function(n_exp){
  X = rnorm(n_exp)
  Y = rnbinom(n_exp, size = 1/3., mu = exp(X/100))
  Y = rpois(n_exp, lambda =  exp(X/100))
  Test = data.frame(X,Y)
  fit <- glm(Y ~ X, data = Test, poisson(link = "log"))
  bread=vcov(fit) 
  score=estfun(fit)
  meat=t(score)%*%score
  sandwich=bread%*%meat%*%bread
  return(list('Naive_Var'=bread['X','X'],
              'Sandwich_Var'= sandwich['X','X'],
              'B' = fit$coefficients['X'],
              'X0' = fit$coefficients['(Intercept)']))
}

n_repl = 10000

simulation = NULL
for(i in 1:n_repl){
  simulation = rbind(simulation,data.frame(replicate_helper(n_exp)))
}


p <- ggplot(simulation) 
p <- p +  geom_density(aes(Naive_Var,colour="MLE Variance"    )) 
p <- p +  geom_density(aes(Sandwich_Var,colour="Sandwich Variance"    )) 
p <- p +  geom_vline(aes(xintercept = var(simulation$B),
                        linetype = "Sample Variance"))
p <- p +  geom_vline(aes(xintercept = mean(simulation$Naive_Var),
                         colour="MLE Variance"))
p <- p +  geom_vline(aes(xintercept = mean(simulation$Sandwich_Var),
                         colour="Sandwich Variance" ))
p <- p +   ggtitle("Distribution of the estimated variance") +
           xlab('Variance') +
           ylab('Density')

p <- p + labs(colour='Distribution of\nestimated values')+
         labs(linetype = 'Mean Value') +
         theme(plot.title = element_text(hjust = 0.5))
p