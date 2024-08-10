#demonstration of likelihood computations
#setup the data
set.seed(12345)
y <- rbinom(1000,1,0.5)
X1 <- rnorm(1000)
X2 <- rnorm(1000, 2, 3)
X3 <- rnorm(1000, 1, 2)
X4 <- rep.int(1, times = 1000)
X <- cbind(y, X1, X2, X3, X4)
head(X)
#setup the likelihood function; the beta is a vector of parameters to be estimated in a function; data is a data matrix with a first
#column as dependent variable; beta must be a column vector (can be done with the rbind function)
logit.ll <- function(beta, data) {
  #obtain linear predictions for a given beta vector
  lp <- data[,2:ncol(data)]%*%beta
  #transform these predictions using logit cdf
  logit <- 1/(1 + exp(-lp))
  #R allows some nice tricks as you can raise vector to a power element-wise by another vector
  ll <- sum(log(logit^data[,1]*(1 - logit)^(1 - data[,1])))
  return(-ll)
}
#now we have the function set; the only remaining thing is to run optim command and obtain standard errors
#first element for the optim function is always initial values
#second is the name of log-likelihood function
#optimization method is optional as they typically produce similar results
#we set hessian to true as we'll need it to extract standard errors
#data argument is inherited by optim automatically from logit.ll
output <- optim(c(1,1,1,1), logit.ll, method = 'BFGS', hessian = TRUE, data=X)
#we can get MLE of parameter estimates
output$par
#we can also get standard errors
t_hessian <- solve(output$hessian)
se <- sqrt(diag(t_hessian))
se
#let's check how these results compare to the standard logit function from R
library(aod)
logit <- glm(y ~ X1 + X2 + X3, data = data.frame(X), family = "binomial")
summary(logit)
output$par
se
#results match exactly as expected (note that in our results constant is the last coefficient reported)