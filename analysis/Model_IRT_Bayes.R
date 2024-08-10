library(R2jags)
#generate artificial data for testing
x1 <- rbinom(100, 3, 0.6) + 1
x2 <- rbinom(100, 1, 0.6)
y <- cbind(x1, x2)
hmod <- list(N = nrow(y), y = as.matrix(y), t0 = rep(0,3), T0 = diag(.1,3))

  
y=data[,c("v95.3","v87")]
y$v87 <- ifelse(y$v87==1,1,0)
y[y==7 | y==8 | y==9] = NA
y = as.data.frame(apply(y,2, function(x) ifelse(is.na(x),1,x)))
#y$v95.3 = y$v95.3 - 1

hmod<-list(N=nrow(data), t0=rep(0,3),T0=diag(.1,3), y=as.matrix(y))


lv_model2 <- function() {
  for (i in 1:N) {
    pi1[i] <- alpha1 + beta1*xi[i]
    probit(pi2[i]) <- alpha2 + beta2*xi[i]
    y[i,1] ~ dordered.probit(pi1[i], tau1[1:3])
    y[i,2] ~ dbern(pi2[i])
    xi[i] ~ dnorm(0,1)
  }
  alpha1 ~ dnorm(0,1)
  beta1 ~ dunif(0,1)
  alpha2 ~ dnorm(0,1)
  beta2 ~ dunif(0,1)
  tausort1 ~ dmnorm(t0, T0)
  tau1 <- sort(tausort1[1:3])
}

params <- c("alpha1", "alpha2", "beta1", "beta2", "xi")
inits <- function() {
  list("tausort1" = c(-0.5, 0, 0.5),
              "alpha1" = 0.1,
              "alpha2" = 0.1,
              "beta1" = 0.1,
              "beta2" = 0.1,
       #here, don't forget to replace 100 to the number of rows in the actual data
              "xi" = rep(0.1, nrow(data)))
}
#this runs the Gibbs sampler
results <- jags(data=hmod, inits=inits, params, n.chains=3, n.iter=1000, n.burnin=500, model.file=lv_model2)

