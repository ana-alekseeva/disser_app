#this file recreates Table 1 from original Moral and Zhirnov paper (table for Norway)
#load data from csv
data <- read.csv("NSD89.csv")
#this function programs the likelihood
#beta is the coefficient vector for both equations
#data is a dataset with the first element (column) as voter's identification number, the second element (column) as voter's party choice
#the third element is the matrix of the first equation variables
#the fourth element is the constant plus the matrix of the second equation variables
#division is the division number that separates first equation parameters from the second one
logit.cccl <- function(beta, data, division) {
  #I decided to eschew loops as vector operations work a bit faster in R
  #first, let's get X_ij*beta for every observation
  betas <- data[,3:(division + 2)]%*%beta[1:division]
  #next, let's get -Z_ij*gamma for every observation
  gammas <- -data[,(division+3):(length(beta)+2)]%*%beta[(division+1):length(beta)]
  #take the log of 1 + exp(-Z_ij*gamma), as was done in the formula; note that constant is included as the column of 1s into
  #this second equation
  second_term <- log(1 + exp(gammas))
  #to compute sums for every voter, first we point-wise multiply exp(X_ij)*(1 + exp(-Z_ij*gamma))^(-1)
  #this gives us the values for every voter-party pair in the data that we can use to compute voter-specific sums
  third_term <- exp(betas)*(1/(1 + exp(gammas)))
  #initialize the dataset with first half of the likelihood calculations done
  data_1 <- data.frame('id' = data[,1], 'y' = data[,2], first_half = betas - second_term)
  #now we only need to get voter-specific sums
  data_2 <- data.frame('id' = data[,1], 'y' = data[,2], 'to_sum' = third_term)
  #which we do by using the function aggregate to sum the variable third_term for every voter
  to_merge <- aggregate(data_2$to_sum, by = list(id = data_2$id), FUN = sum)
  #merge computed voter-specific sums back
  data_1 <- merge(data_1, to_merge, by=c('id'))
  #now we only need to subtract log of the computed sums from the first half and multiply these values by the dependent variable
  #to make sure that we are summing log-likelihoods for observations that correspond to the actual party choice, as per the formula
  #data_1[,'x'] is due to idiosyncratic characteristics of aggregate function, as computed variable is always called x
  #by this function for some reason
  return(-sum((data_1[,'first_half'] - log(data_1[,'x']))*data_1[,'y']))
}
#define the data to use in the model estimation
dataset <- cbind(data$id, data$choice, data$chprox1, data$chprox2, data$chprox3, data$chprox4, data$chprox5,
                      data$chprox6, data$chprox7, 1, data$constvar1, data$constvar2, data$constvar3)
dataset <- cbind(data_1$id, data_1$choice, data_1$chprox1, data_1$chprox2, data_1$chprox3, data_1$chprox4, data_1$chprox5,
                 data_1$chprox6, data_1$chprox7, 1, data_1$constvar1, data_1$constvar2, data_1$constvar3)
    #use optim function to get the estimates
output <- optim(rep(1, times = 11), logit.cccl, method = 'BFGS', hessian = TRUE, data=dataset, division = 7)
#test whether converged or not
output$convergence == 0
#check whether parameters are the same as in the paper - they mostly are, except for some very minor differences
output$par
#get the standard errors - also very similar; we can use them to test hypotheses and construct confidence intervals
t_hessian <- solve(output$hessian)
se <- sqrt(diag(t_hessian))
se  