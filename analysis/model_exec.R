setwd("/home/anastasia/Documents/Disser/data")
source("/home/anastasia/Documents/Disser/code/disser_functions.R") 

df <- read.csv("prepared_data.csv")
aggregate(df$left_right_party, by=list(df$party_code),FUN=mean)

df[df$vid==dupl[1],]




library(ggplot2)
library(car)
library(ggpubr)
library(xtable)
library(reshape2)
#############################
# Exploratory factor analysis
#############################

# 1. Factor analysis with categorical variables
#without "v94.2"
df_cat <-df[df$choice==1,] 
df_cat <-df_cat[,c("vid","v22", "v24", "v31.1","v31.2","v94.1","v95.1","v95.2","v95.3","v95.4","v95.5","v95.6","v95.7","v87","v90","v116","v117","v132" )] 
df_cat[,-c(1)][df_cat[,-c(1)]>5] <- NA      
mat_cat <- df_cat[,-c(1)]

library(psych)
library(polycor)
pc <- hetcor(mat_cat, ML=TRUE) 
poly_model = fa(pc$correlations, nfactor=6, cor="poly", fm="mle", rotate = "varimax")
poly_model$loadings

# 2. Factor analysis with dummy variables
#without "v94.2"
df_dummy <- df[df$choice==1,] 
df_dummy <-df_dummy[,c("vid","v22", "v24", "v31.1","v31.2","v94.1","v95.1","v95.2","v95.3","v95.4","v95.5","v95.6","v95.7","v87","v90","v116","v117","v132" )] 
df_dummy[,-c(1)][df_dummy[,-c(1)]>5] <- NA       
df_dummy$vid <- NULL

df_dummy[df_dummy<3] <- 1
df_dummy[df_dummy>=3] <- 0
df_dummy[is.na(df_dummy)] <- 1

fact_res_dummy <- factanal(df_dummy, 5, rotation="varimax")
print(fact_res_dummy)

# 3. Factor analysis with continuous variables

df_cont <- df[df$choice==1,]
df_cont <- df_cont[,c("vid","v22", "v24", "v31.1","v31.2","v94.1","v95.1","v95.2","v95.3","v95.4","v95.5","v95.6","v95.7","v87","v90","v116","v117","v132" )] 
df_cont[,-c(1)][df_cont[,-c(1)]>5] <- NA
library(car)

df_cont[,c("v22", "v24","v116", "v117","v132")] <- apply(df_cont[,c("v22", "v24","v116", "v117","v132")], 2,function (x) recode(x, "1= 10; 2=7.25; 3=5.5; 4=3.75; 5=1"))
df_cont[,c("v22", "v24","v116", "v117","v132")] <- apply(df_cont[,c("v22", "v24","v116", "v117","v132")], 2,function (x) ifelse(is.na(x), mean(x, na.rm=TRUE), x))

df_cont[,c("v31.1", "v31.2","v94.1",  "v95.1", "v95.2", "v95.3", "v95.4", "v95.5", "v95.6", "v95.7")] <- apply( 
  df_cont[,c("v31.1", "v31.2","v94.1", "v95.1", "v95.2", "v95.3", "v95.4", "v95.5", "v95.6", "v95.7")], 2,function (x) recode(x, "1= 10; 2=7; 3=4; 4=1"))
df_cont[,c("v31.1", "v31.2","v94.1",  "v95.1", "v95.2", "v95.3", "v95.4", "v95.5", "v95.6", "v95.7")] <- apply(
  df_cont[,c("v31.1", "v31.2","v94.1",  "v95.1", "v95.2", "v95.3", "v95.4", "v95.5", "v95.6", "v95.7")],2, function (x) ifelse(is.na(x), mean(x, na.rm=TRUE), x))

df_cont$v87 <- ifelse(df_cont$v87<2 | is.na(df_cont$v87), 1, 0) 
df_cont$v90 <- ifelse(df_cont$v90<3 | is.na(df_cont$v90), 1, 0) 

fact_res_cont <- factanal(df_cont[,-c(1)], 5, rotation="varimax")
print(fact_res_cont)



#######################################################################
get_fact_scores <- function(df_factors, fact.res, fact_num){
  fact_scores <- c()
  for(i in c(1:fact_num)){
    fact_scores <- rbind(fact_scores,fact.res$loadings[,i] %*% t(df_factors[,-c(1)]))
  }
  fact_scores <- as.data.frame(t(fact_scores))
  fact_scores$vid <- df_factors[,1]
return(fact_scores)}

fact_scores <- get_fact_scores(df_cont,fact_res_cont,5)

fact_scores <- do.call("rbind", replicate(6, fact_scores, simplify = FALSE))
fact_scores <- fact_scores[order(fact_scores$vid),]

write.csv(fact_scores,"fact_scores.csv", row.names=F)

#############################
#Confirmatory factor analysis
#############################

library(R2jags)
set.seed(123)

df_lv <- df[df$choice==1,]

#correlation matrix
#std matrix 

# 1.Western countries
# v90(1,2=1,3,4=0), v94.1 (1-4)


y1=df_lv[,c("v94.1","v90")]
y1$v90 <- ifelse(y1$v90==1 | y1$v90==2, 1,0)
y1$v94.1 <-ifelse(y1$v94.1>4,NA,y1$v94.1) 
y1$v94.1 <- recode(y1$v94.1, "1=4;2=3;3=2;4=1")

hmod1<-list(N=nrow(y1), t0=rep(0,3),T0=diag(.1,3), y=as.matrix(y1))

lv_model1 <- function() {
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
       "xi" = rep(0.1, nrow(y1)))
}
#this runs the Gibbs sampler
res1<- jags(data=hmod1, inits=inits, params, n.chains=3, n.iter=200000, n.burnin=100000, model.file=lv_model1)
save(res1, file = "lv_west_countries_new.RData")

lv_sum <- res1$BUGSoutput$summary[1:4,]
west_countries <- res1$BUGSoutput$mean$xi
west_countries <- as.data.frame(cbind(vid = df_lv$vid,party_code=df_lv$party_code,west_countries))
west_countries_groups <- aggregate(west_countries,by=list(west_countries$party_code), mean)


load("lv_west_countries_new.RData")

load("lv_west_countries_new.RData")
s <- res1$BUGSoutput$summary
a <- as.mcmc(res1)
gelman.diag(a)

plot(res1$BUGSoutput$sims.list$beta2,type="l")

# 2. Democracy and Freedom
# v31.1, v95.5, v95.1, v116

y2=df_lv[,c("v31.1","v95.5","v95.1","v116")]
y2[y2>5] <- NA
y2[,c("v31.1","v95.5","v95.1")] <- apply(y2[,c("v31.1","v95.5","v95.1")], 2,function (x) recode(x, "1=4; 2=3; 3=2; 4=1"))

hmod2<-list(N=nrow(y2), t0=rep(0,3),T0=diag(.1,3),t02=rep(0,4),T02=diag(.1,4), y=as.matrix(y2))

lv_model2 <- function() {
  for (i in 1:N) {
    pi1[i] <- alpha1 + beta1*xi[i]
    y[i,1] ~ dordered.probit(pi1[i], tau1[1:3])
    pi2[i] <- alpha2 + beta2*xi[i]
    y[i,2] ~ dordered.probit(pi2[i], tau1[1:3])
    pi3[i] <- alpha3 + beta3*xi[i]
    y[i,3] ~ dordered.probit(pi3[i], tau1[1:3])
    
    pi4[i]<- alpha4 + beta4*xi[i]
    y[i,4] ~ dordered.probit(pi4[i], tau2[1:4])
    xi[i] ~ dnorm(0,1)
  }
  alpha1 ~ dnorm(0,1)
  beta1 ~ dunif(0,1)
  alpha2 ~ dnorm(0,1)
  beta2 ~ dunif(0,1)
  alpha3 ~ dnorm(0,1)
  beta3 ~ dunif(0,1)
  alpha4 ~ dnorm(0,1)
  beta4 ~ dunif(0,1)
  
  tausort1 ~ dmnorm(t0, T0)
  tau1 <- sort(tausort1[1:3])
  tausort2 ~ dmnorm(t02, T02)
  tau2 <- sort(tausort2[1:4])
}

params <- c("alpha1", "alpha2", "alpha3","alpha4","beta1", "beta2","beta3","beta4", "xi")
inits <- function() {
  list("tausort1" = c(-0.5, 0, 0.5),
       "tausort2" = c(-0.75,-0.25,0.25,0.75),
       "alpha1" = 0.1,
       "alpha2" = 0.1,
       "alpha3" = 0.1,
       "alpha4" = 0.1,
       "beta1" = 0.1,
       "beta2" = 0.1,
       "beta3" = 0.1,
       "beta4" = 0.1,
       #here, don't forget to replace 100 to the number of rows in the actual data
       "xi" = rep(0.1, nrow(y2)))
}
#this runs the Gibbs sampler
res2<- jags(data=hmod2, inits=inits, params, n.chains=3, n.iter=400000, n.burnin=200000, model.file=lv_model2)
save(res2, file = "lv_democracy_new.RData")

load("lv_democracy_new.RData")
s <- res2$BUGSoutput$summary
a <- as.mcmc(res2)
gelman.diag(a)

res2$BUGSoutput$summary
lv_sum <- rbind(lv_sum, res2$BUGSoutput$summary[1:8,])

democracy <- res2$BUGSoutput$mean$xi
democracy <- as.data.frame(cbind(vid = df_lv$vid,party_code=df_lv$party_code,democracy))
democracy_groups <- aggregate(democracy,by=list(democracy$party_code), mean)

# 3. Socialism and USSR
# v95.2, v117, v87(1=1,2,3=0)

y3=df_lv[,c("v95.2","v87","v117")]
y3[y3>5] <- NA
y3[,c("v95.2")] <- recode(y3[,c("v95.2")], "1=4; 2=3; 3=2; 4=1")
y3[,c("v117")] <- recode(y3[,c("v117")], "1=5; 2=4; 3=3; 4=2; 5=1")
y3$v87 <- ifelse(y3$v87==1,1,0)


hmod3<-list(N=nrow(y3), t0=rep(0,3),T0=diag(.1,3),t02=rep(0,4),T02=diag(.1,4), y=as.matrix(y3))

lv_model3 <- function() {
  for (i in 1:N) {
    pi1[i] <- alpha1 + beta1*xi[i]
    y[i,1] ~ dordered.probit(pi1[i], tau1[1:3])
    
    probit(pi2[i]) <- alpha2 + beta2*xi[i]
    y[i,2] ~ dbern(pi2[i])
    
    pi3[i]<- alpha3 + beta3*xi[i]
    y[i,3] ~ dordered.probit(pi3[i], tau2[1:4])
    xi[i] ~ dnorm(0,1)
  }
  alpha1 ~ dnorm(0,1)
  beta1 ~ dunif(0,1)
  alpha2 ~ dnorm(0,1)
  beta2 ~ dunif(0,1)
  alpha3 ~ dnorm(0,1)
  beta3 ~ dunif(0,1)
  
  tausort1 ~ dmnorm(t0, T0)
  tau1 <- sort(tausort1[1:3])
  tausort2 ~ dmnorm(t02, T02)
  tau2 <- sort(tausort2[1:4])
}

params <- c("alpha1", "alpha2", "alpha3","beta1", "beta2","beta3", "xi")
inits <- function() {
  list("tausort1" = c(-0.5, 0, 0.5),
       "tausort2" = c(-0.75,-0.25,0.25,0.75),
       "alpha1" = 0.1,
       "alpha2" = 0.1,
       "alpha3" = 0.1,
       "beta1" = 0.1,
       "beta2" = 0.1,
       "beta3" = 0.1,
       #here, don't forget to replace 100 to the number of rows in the actual data
       "xi" = rep(0.1, nrow(y3)))
}
#this runs the Gibbs sampler
res3<- jags(data=hmod3, inits=inits, params, n.chains=3, n.iter=100000, n.burnin=50000, model.file=lv_model3)
save(res3, file = "lv_socialism_new.RData")

load("lv_socialism.RData")

library(coda)
a <- as.mcmc(res3)
geweke.plot(a)
geweke.diag(a)
raftery.diag(a)
gelman.diag(a)

res3$BUGSoutput$summary
lv_sum <- rbind(lv_sum, res3$BUGSoutput$summary[1:6,])


load("lv_socialism_new.RData")
s <- res3$BUGSoutput$summary
a <- as.mcmc(res3)
gelman.diag(a)

socialism <- res3$BUGSoutput$mean$xi
socialism <- as.data.frame(cbind(vid = df_lv$vid,party_code=df_lv$party_code,socialism))
socialism_groups <- aggregate(socialism,by=list(socialism$party_code), mean)

plot(res3$BUGSoutput$sims.list$alpha1, type="l")
plot(res3$BUGSoutput$sims.list$alpha2, type="l")
plot(res3$BUGSoutput$sims.list$alpha3, type="l")
plot(res3$BUGSoutput$sims.list$beta1, type="l")
plot(res3$BUGSoutput$sims.list$beta2, type="l")
plot(res3$BUGSoutput$sims.list$beta3, type="l")

  # 4. Free Market
#v87 (3=1,1,2=0), v95.4, v95.3

y4=df_lv[,c("v95.3","v95.4")]
y4[y4>5] <- NA
y4$v95.4 <- recode(y4[,c("v95.4")], "1=4; 2=3; 3=2; 4=1")

hmod4<-list(N=nrow(y4), t0=rep(0,3),T0=diag(.1,3), y=as.matrix(y4))

lv_model4 <- function() {
  for (i in 1:N) {
    pi1[i] <- alpha1 + beta1*xi[i]
    y[i,1] ~ dordered.probit(pi1[i], tau1[1:3])
    
    pi2[i]<- alpha2 + beta2*xi[i]
    y[i,2] ~ dordered.probit(pi2[i], tau1[1:3])
    xi[i] ~ dnorm(0,1)
  }
  alpha1 ~ dnorm(0,1)
  beta1 ~ dunif(0,1)
  alpha2 ~ dnorm(0,1)
  beta2 ~ dunif(0,1)

  tausort1 ~ dmnorm(t0, T0)
  tau1 <- sort(tausort1[1:3])
}

params <- c("alpha1", "alpha2","beta1", "beta2", "xi")
inits <- function() {
  list("tausort1" = c(-0.5, 0, 0.5),
       "alpha1" = 0.1,
       "alpha2" = 0.1,
       "beta1" = 0.1,
       "beta2" = 0.1,
       #here, don't forget to replace 100 to the number of rows in the actual data
       "xi" = rep(0.1, nrow(y4)))
}
#this runs the Gibbs sampler
res4<- jags(data=hmod4, inits=inits, params, n.chains=3, n.iter=100000, n.burnin=50000, model.file=lv_model4)
save(res4, file = "lv_market_new.RData")

load("lv_market.RData")

load("lv_market_new.RData")
s <- res4$BUGSoutput$summary
a <- as.mcmc(res4)
gelman.diag(a)

plot(res4$BUGSoutput$sims.list$alpha1, type="l")
plot(res4$BUGSoutput$sims.list$alpha2, type="l")
plot(res4$BUGSoutput$sims.list$beta1, type="l")
plot(res4$BUGSoutput$sims.list$beta2, type="l")



lv_sum <- rbind(lv_sum, res4$BUGSoutput$summary[1:4,])

xtable(lv_sum)

a <- res4$BUGSoutput$mean
market <- res4$BUGSoutput$mean$xi
market <- as.data.frame(cbind(vid = df_lv$vid,party_code=df_lv$party_code,market))
market_groups <- aggregate(market,by=list(market$party_code), mean)

# add putin approval (v22) and nationalism (v95.7)
df_lv$v22 <- ifelse(df_lv$v22>5,NA,df_lv$v22)
df_lv$v22 <- recode(df_lv$v22,"1=5;2=4;3=3;4=2;5=1")
df_lv$v22 <- ifelse(is.na(df_lv$v22),mean(df_lv$v22, na.rm=T),df_lv$v22)
df_lv$v95.7 <- ifelse(df_lv$v95.7>4,NA,df_lv$v95.7)
df_lv$v95.7 <- recode(df_lv$v95.7,"1=4;2=3;3=2;4=1")
df_lv$v95.7 <- ifelse(is.na(df_lv$v95.7),mean(df_lv$v95.7,na.rm=T),df_lv$v95.7)

latent_variables <- as.data.frame(cbind(vid=df_lv$vid,west_countries=west_countries[,-c(1,2)],democracy=democracy[,-c(1,2)],socialism=socialism[,-c(1,2)],market=market[,-c(1,2)],putin = df_lv$v22, nationalism=df_lv$v95.7))
latent_variables <- do.call("rbind", replicate(6, latent_variables, simplify = FALSE))
latent_variables <- latent_variables[order(latent_variables$vid),]
write.csv(latent_variables, "latent_variables_new.csv", row.names = F)

#################################################
#MODEL EXECUTION
#################################################

#1. WITH EXPL. FACTORS

fact_scores <- read.csv("fact_scores.csv")
data <- read.csv("prepared_data.csv")
party_scores <- cbind(data,fact_scores)
party_scores <- party_scores[party_scores$choice==1,]
party_scores <- aggregate(party_scores, by=list(party_scores$party_code), mean)[,c("V1","V2","V3","V4","V5")]
party_scores <-do.call("rbind", replicate(nrow(data)/6, party_scores, simplify = FALSE))

Nm_all <- colMeans(fact_scores[,-6])

Nm=0
pol_ext <- t(apply(party_scores,1, function(x) (x-Nm_all)^2))
pol_ext <- sqrt(rowSums(pol_ext))

#----------------------------------------------------
eucl_dist <-  (fact_scores[,-6]- party_scores)^2
#----------------------------------------------------
Nm <- 0
scal_prod <-(fact_scores[,-6]-Nm)*(party_scores - Nm)
#-----------------------------------------------------
left_right_euc <- (data$left_right_party-data$left_right_resp)^2

# 1.1. EUCLIDEAN DISTANCE 
# Without strong affinity

data_cccl_euc <- cbind(data$vid,data$choice,left_right_euc, eucl_dist, 1, data$elect_viability,pol_ext, data$strong_aff_1,  
                       data$media_repr_mean,
                      data$main_inf_source_radio,data$main_inf_source_newspapers,data$main_inf_source_internet)

data_cccl_euc[,c(10,11,13)] <- apply(data_cccl_euc[,c(10,11,13)],2, function(x) scale(x))

data_cccl_euc <- as.matrix(data_cccl_euc)

output_euc <- optim(rep(1, times = 14), logit.cccl, method = 'BFGS', hessian = TRUE, data=data_cccl_euc, division = 6)
output_euc$convergence == 0
par_euc <- output_euc$par
t_hessian <- solve(output_euc$hessian)
se <- sqrt(diag(t_hessian))
output_euc$par/se
          


data_cccl_euc <- cbind(data$vid,data$choice,left_right_euc, eucl_dist, 1, data$strong_aff_1)
                       

#data_cccl_euc[,c(3)] <- scale(data_cccl_euc[,c(3)])

data_cccl_euc <- as.matrix(data_cccl_euc)

output_euc <- optim(rep(1, times = 8), logit.cccl, method = 'BFGS', hessian = TRUE, data=data_cccl_euc, division = 6)
output_euc$convergence == 0
par_euc <- output_euc$par
t_hessian <- solve(output_euc$hessian)
se <- sqrt(diag(t_hessian))
output_euc$par/se



#################################################################################

# 1.2. SCALAR PRODUCT 
# With strong affinity

data_cccl_scal <- cbind(data$vid,data$choice,left_right_euc, scal_prod, 1, data$elect_viability,pol_ext, data$strong_aff_1,
                       data$media_repr_mean,
                       data$main_inf_source_radio,data$main_inf_source_newspapers,data$main_inf_source_internet)

data_cccl_scal[,c(3,10,11,13)] <- apply(data_cccl_scal[,c(3,10,11,13)],2, function(x) scale(x))

data_cccl_scal <- as.matrix(data_cccl_scal)

output_scal <- optim(rep(1, times = 14), logit.cccl, method = 'BFGS', hessian = TRUE, data=data_cccl_scal, division = 6)
output_scal$convergence == 0
par_scal <- output_scal$par
t_hessian <- solve(output_scal$hessian)
se <- sqrt(diag(t_hessian))
output$par/se




#######################################################################################

#2 WITH CONFIRMATORY FACTORS

lv <- read.csv("latent_variables_new.csv")

lv[,c("west_countries","democracy","socialism","market","putin","nationalism")] <- scale(lv[,c("west_countries","democracy","socialism","market","putin","nationalism")])
summary(lv)

data <- read.csv("prepared_data.csv")
party_scores_lv <- cbind(data[,c("party_code","choice")],lv,data$left_right_party)
party_scores_lv <- party_scores_lv[party_scores_lv$choice==1,]
party_scores_lv <- aggregate(party_scores_lv,by=list(party_scores_lv$party_code), mean)
  party_scores_lv <- aggregate(party_scores_lv, by=list(party_scores_lv$party_code), mean)[,c("west_countries","democracy","socialism","market","putin","nationalism")]
party_scores_lv <-do.call("rbind", replicate(nrow(data)/6, party_scores_lv, simplify = FALSE))

xtable(party_scores_lv)

Nm_all <- colMeans(lv[,-1])
# means for all respondents
pol_ext_lv <- t(apply(party_scores_lv,1, function(x) (x-Nm_all)^2))
summary(pol_ext_lv)
pol_ext_lv <- sqrt(rowSums(pol_ext_lv))
pol_ext_lv[1:6]
#----------------------------------------------------
eucl_dist_lv <-  (lv[,-1]- party_scores_lv)^2
#----------------------------------------------------
scal_prod_lv <-(lv[,-1]-Nm_all)*(party_scores_lv - Nm_all)
#-----------------------------------------------------
left_right_euc <- (data$left_right_party-data$left_right_resp)^2
Nm <- colMeans(data[,c("left_right_party","left_right_resp")])
left_right_sc <- (data$left_right_party-Nm[1])*(data$left_right_resp-Nm[2])

# 2.1. EUCLIDEAN DISTANCE 

#PURE

data_cccl_lv_euc1 <- cbind(data$vid,data$choice,left_right_euc, eucl_dist_lv, 1, data$elect_viability,pol_ext_lv, data$strong_aff_1)
write.csv(data_cccl_lv_euc1,"data_cccl_lv_euc1_new.csv", row.names=FALSE)

#data_cccl_lv_euc1[,c(11,12,14)] <- apply(data_cccl_lv_euc[,c(11,12,14)],2, function(x) scale(x))

data_cccl_lv_euc1 <- as.matrix(data_cccl_lv_euc1)

output_lv_euc1 <- optim(rep(1, times = 11), logit.cccl, method = 'BFGS', hessian = TRUE, data=data_cccl_lv_euc1, division = 7)
save(output_lv_euc1,file="output_cccl_lv_euc1_new.RData")

output_lv_euc1$convergence == 0



get_sign <- function(output){
  t_hessian <- solve(output$hessian)
  se <- sqrt(diag(t_hessian))
  z <- output$par/se 
  return(z)
}

z <- get_sign(output_lv_euc1)
round(2*pnorm(-abs(z)),3)

table <- data.frame("vars"=c("Left-Right","","West Countries as Enemy","","Democracy","","Socialism and USSR","",
                             "Market Economy","","Putin's Approval","","Nationalism","", 
                             "Intercept","","Electoral Viability","","Policy Extremity","","Strong Affinity","",
                             "Media Representation","","Radio","","Newspapers","", "Internet","",
                             "TV*Main_Source_TV","","Radio*Main_Source_Radio","","Newspapers*Main_Source_Newspapers","", "Internet*Main_Source_Internet","",
                             "Log-likelihood","AIC","BIC","Observations"))
table <- cbind(table, CCCL_euc1 =make_table(output_lv_euc1, k=11,obs=6144,n=42)[,-1])


#-----------------------------------------------------
# MEDIA
df_media <- data[,c("vid","party_code","choice","media_repr_mean","main_inf_source_tv" ,"main_inf_source_radio" ,"main_inf_source_newspapers","main_inf_source_internet")]
df_media$media_repr_mean <- df_media$media_repr_mean/(sum(df_media$media_repr_mean)/(nrow(df_media)/6))

data_cccl_lv_euc2 <- cbind(data$vid,data$choice,left_right_euc, eucl_dist_lv, 1, data$elect_viability,pol_ext_lv, data$strong_aff_1,  
                           df_media$media_repr_mean)

#data_cccl_lv_euc2[,c(14)] <- scale(data_cccl_lv_euc2[,c(14)])
write.csv(data_cccl_lv_euc2,"data_cccl_lv_euc2_new.csv",row.names = F)

data_cccl_lv_euc2 <- as.matrix(data_cccl_lv_euc2)

output_lv_euc2 <- optim(rep(1, times = 12), logit.cccl, method = 'BFGS', hessian = TRUE, data=data_cccl_lv_euc2, division = 7)
save(output_lv_euc2,file="output_cccl_lv_euc2_new.RData")

output_lv_euc2$convergence == 0

table <- cbind(table, CCCL_euc2=make_table(output_lv_euc2, k=12,obs=6144,n=42)[,-1])

#-----------------------------
# MEDIA INTERACTION
df_media$TV <- rep(c(3445.95,3156.18,3818.44,1613.39,10292.89,-72.25),nrow(df_media)/6)
df_media$Radio <- rep(c(1172.02,1780.78,-14.4099999999999,-323.27,-5409.92,940.55),nrow(df_media)/6)
df_media$Newspapers <- rep(c(971.74,141.67,466.4,512,-9232.06,181.43),nrow(df_media)/6)
df_media$Internet <- rep(c(1036.54,1980.21,1336.55,673.86,6953.41,531.26),nrow(df_media)/6)

df_media$TV <- log(abs(df_media$TV))*sign(df_media$TV)
df_media$Radio <- log(abs(df_media$Radio))*sign(df_media$Radio)
df_media$Newspapers <- log(abs(df_media$Newspapers))*sign(df_media$Newspapers)
df_media$Internet <- log(abs(df_media$Internet))*sign(df_media$Internet)

#df_inter <- df_media[,c("main_inf_source_tv" ,"main_inf_source_radio" ,"main_inf_source_newspapers","main_inf_source_internet")]*df_media[,c("TV" ,"Radio" ,"Newspapers","Internet")] 

df_media$TV_int <- df_media$main_inf_source_tv*df_media$TV
df_media$Radio_int <- df_media$main_inf_source_radio*df_media$Radio
df_media$Newspapers_int <- df_media$main_inf_source_newspapers*df_media$Newspapers
df_media$Internet_int <- df_media$main_inf_source_internet*df_media$Internet
df_media$inter <-df_media$TV_int+df_media$Radio_int+df_media$Newspapers_int+df_media$Internet_int 

data_cccl_lv_euc3 <- cbind(data$vid,data$choice,left_right_euc, eucl_dist_lv, 1, data$elect_viability,pol_ext_lv, data$strong_aff_1, 
                        df_media$main_inf_source_radio,df_media$main_inf_source_newspapers,df_media$main_inf_source_internet,
                         #df_media$inter)
                        # df_media$TV, df_media$Radio,df_media$Newspapers,df_media$Internet,
                        df_media$TV_int ,
                          df_media$Radio_int, df_media$Newspapers_int,df_media$Internet_int)

write.csv(data_cccl_lv_euc3,"data_cccl_lv_euc3_new.csv",row.names = F)
data_cccl_lv_euc3 <- read.csv("data_cccl_lv_euc3.csv")
data_cccl_lv_euc3 <- as.matrix(data_cccl_lv_euc3)

output_lv_euc3 <- optim(rep(1, times = 18), logit.cccl, method = 'BFGS', hessian = TRUE, data=data_cccl_lv_euc3, division = 7)
save(output_lv_euc3,file="output_cccl_lv_euc3_new.RData")

output_lv_euc3$convergence == 0
hessian <- solve(output_lv_euc3$hessian)
se=sqrt(diag(hessian))

table <- cbind(table, CCCL_Euc3 = make_table(output_lv_euc3, k=18,obs=6144,n=42)[,-1])
xtable(table)
    

#------------

# 2.2. SCALAR PRODUCT 

data_cccl_lv_sc1 <- cbind(data$vid,data$choice,left_right_sc, scal_prod_lv, 1, data$elect_viability,pol_ext_lv, data$strong_aff_1)
write.csv(data_cccl_lv_sc1,"data_cccl_lv_sc1_new.csv", row.names=FALSE)

data_cccl_lv_sc1 <- as.matrix(data_cccl_lv_sc1)

output_lv_sc1 <- optim(rep(1, times = 11), logit.cccl, method = 'BFGS', hessian = TRUE, data=data_cccl_lv_sc1, division = 7)
save(output_lv_sc1,file="output_cccl_lv_sc1_new.RData")

output_lv_sc1$convergence == 0

table <- data.frame("vars"=c("Left-Right","","West Countries as Enemy","","Democracy","","Socialism and USSR","",
                             "Market Economy","","Putin's Approval","","Nationalism","", 
                             "Intercept","","Electoral Viability","","Policy Extremity","","Strong Affinity","",
                             "Media Representation","","Radio","","Newspapers","", "Internet","",
                             "TV*Main_Source_TV","","Radio*Main_Source_Radio","","Newspapers*Main_Source_Newspapers","", "Internet*Main_Source_Internet","",
                             "Log-likelihood","AIC","BIC","Observations"))
table <- cbind(table, CCCL_Sc1 =make_table(output_lv_sc1, k=11,obs=6144,n=42)[,-1])

#-------------------------------------------------------------
#!!!! scaled media representation
data_cccl_lv_sc2 <- cbind(data$vid,data$choice,left_right_sc, scal_prod_lv, 1, data$elect_viability,pol_ext_lv, data$strong_aff_1,  
                           df_media$media_repr_mean)
write.csv(data_cccl_lv_sc2,"data_cccl_lv_sc2_new.csv",row.names = F)

data_cccl_lv_sc2 <- as.matrix(data_cccl_lv_sc2)

output_lv_sc2 <- optim(rep(1, times = 12), logit.cccl, method = 'BFGS', hessian = TRUE, data=data_cccl_lv_sc2, division = 7)
save(output_lv_sc2,file="output_cccl_lv_sc2_new.RData")
output_lv_sc2$convergence == 0

table <- cbind(table, CCCL_Sc2=make_table(output_lv_sc2, k=12,obs=6144,n=42)[,-1])
a <- make_table(output_lv_sc2, k=12,obs=6144,n=42)

xtable(table)

#-------------------------------------------------------
data_cccl_lv_sc3 <- cbind(data$vid,data$choice,left_right_sc, scal_prod_lv, 1, data$elect_viability,pol_ext_lv, data$strong_aff_1, 
                           df_media$main_inf_source_radio,df_media$main_inf_source_newspapers,df_media$main_inf_source_internet,
                           df_media$TV_int ,
                           df_media$Radio_int, df_media$Newspapers_int,df_media$Internet_int)

write.csv(data_cccl_lv_sc3,"data_cccl_lv_sc3.csv",row.names = F)

data_cccl_lv_sc3 <- as.matrix(data_cccl_lv_sc3)

output_lv_sc3 <- optim(rep(1, times = 18), logit.cccl, method = 'BFGS', hessian = TRUE, data=data_cccl_lv_sc3, division = 7)
#save(output_lv_euc3,file="output_cccl_lv_euc3.RData")

output_lv_sc3$convergence == 0
output_lv_sc3$par
t_hessian <- solve(output_lv_sc3$hessian)
se <- sqrt(diag(t_hessian))
output$par/se 

table <- cbind(table, CCCL_Sc3 = make_table(output_lv_sc3, k=18,obs=6144,n=42)[,-1])
xtable(table)

###################################################################
#EASY CONDITIONAL LOGIT
###################################################################

# EUCLIDEAN DISTANCE

data_euc <- read.csv("data_cccl_lv_euc1_new.csv")
colnames(data_euc)[2] <- "choice"
data_euc <- data_euc[,-10]
data_euc <- as.matrix(data_euc)

output_euc <- optim(rep(1,10), conditional.logit, method = 'BFGS', hessian = TRUE, data=data_euc)
save(output_euc,file="output_euc.RData")
output_euc$convergence==0

table <- cbind(table, CL_Euc = make_table(output_euc, k=10,obs=6144,n=42)[,-1])

# SCALAR PRODUCT

data_sc<- read.csv("data_cccl_lv_sc1_new.csv")
colnames(data_sc)[2] <- "choice"
data_sc <- data_sc[,-10]
data_sc <- as.matrix(data_sc)

output_sc <- optim(rep(1,10), conditional.logit, method = 'BFGS', hessian = TRUE, data=data_sc)
save(output_sc,file="output_sc.RData")
output_sc$convergence==0
table <- cbind(table, CL_Sc = make_table(output_sc, k=10,obs=6144,n=42)[,-1])
xtable(table)

#############################################################
#Calculate Predicted Vote Shares
#############################################################
setwd("/home/anastasia/Documents/Disser/data")
source("/home/anastasia/Documents/Disser/code/disser_functions.R") 
library(ggplot2)
library(car)
library(ggpubr)
library(xtable)
library(reshape2)


df_ideologies <- read.csv("df_ideologies.csv")
load("output_cccl_lv_euc1_new.RData")
load("output_cccl_lv_euc2_new.RData")
load("output_cccl_lv_euc3.RData")
load("output_cccl_lv_sc1_new.RData")
load("output_cccl_lv_sc2_new.RData")
load("output_euc.RData")
load("output_sc.RData")
data_cccl_lv_euc1 <- read.csv("data_cccl_lv_euc1_new.csv")
data_cccl_lv_euc2 <- read.csv("data_cccl_lv_euc2_new.csv")
data_cccl_lv_euc3 <- read.csv("data_cccl_lv_euc3.csv")
data_cccl_lv_sc1 <- read.csv("data_cccl_lv_sc1_new.csv")
data_cccl_lv_sc2 <- read.csv("data_cccl_lv_sc2_new.csv")

par_euc1 <- output_lv_euc1$par
par_euc2 <- output_lv_euc2$par
par_euc3 <- output_lv_euc3$par
par_sc1 <- output_lv_sc1$par
par_sc2 <- output_lv_sc2$par

par_cl_euc <- output_euc$par
par_cl_sc <- output_sc$par


# Calculating vote shares
parties_vect <- rep(c("JR","LDPR","CPRF","Yabloko","UR","RC"),nrow(data_cccl_lv_euc1)/6)
parties_vect <- rep(c("СР","ЛДПР","КПРФ","Яблоко","ЕР","Правое дело"),nrow(data_cccl_lv_euc1)/6)

probs_euc1 <- calc_prob_cccl(data_cccl_lv_euc1,par_euc1, division=7)
vote_shares_euc1 <- calc_vote_shares(cbind(data_cccl_lv_euc1[,c(1,2)],parties_vect,probs_euc1))

probs_euc2 <- calc_prob_cccl(data_cccl_lv_euc2,par_euc2, division=7)
vote_shares_euc2 <- calc_vote_shares(cbind(data_cccl_lv_euc2[,c(1,2)],parties_vect,probs_euc2))

probs_euc3 <- calc_prob_cccl(data_cccl_lv_euc3,par_euc3, division=7)
vote_shares_euc3 <- calc_vote_shares(cbind(data_cccl_lv_euc2[,c(1,2)],parties_vect,probs_euc3))

probs_sc1 <- calc_prob_cccl(data_cccl_lv_sc1,par_sc1, division=7)
vote_shares_sc1 <- calc_vote_shares(cbind(data_cccl_lv_sc1[,c(1,2)],parties_vect,probs_sc1))

probs_sc2 <- calc_prob_cccl(data_cccl_lv_sc2,par_sc2, division=7)
vote_shares_sc2 <- calc_vote_shares(cbind(data_cccl_lv_sc2[,c(1,2)],parties_vect,probs_sc2))

probs_cl_euc <- calc_prob_cl(data_cccl_lv_euc1[,-10],par_cl_euc)
vote_shares_cl_euc <- calc_vote_shares(cbind(data_cccl_lv_euc1[,c(1,2)],parties_vect,probs_cl_euc))

probs_cl_sc <- calc_prob_cl(data_cccl_lv_sc1[,-10],par_cl_sc)
vote_shares_cl_sc <- calc_vote_shares(cbind(data_cccl_lv_sc2[,c(1,2)],parties_vect,probs_cl_sc))

table_pred <- cbind(Party= vote_shares_euc1$party,Observed=vote_shares_euc1$real_vote_shares, cccl_Euc1=vote_shares_euc1$predicted_vote_shares,cccl_Euc2=vote_shares_euc2$predicted_vote_shares,cccl_Euc3=vote_shares_euc3$predicted_vote_shares,
                    cccl_Sc1=vote_shares_sc1$predicted_vote_shares,cccl_Sc2=vote_shares_sc2$predicted_vote_shares,
                    cl_Euc=vote_shares_cl_euc$predicted_vote_shares,cl_Sc=vote_shares_cl_sc$predicted_vote_shares)

table_pred <- cbind(Party= vote_shares_euc1$party,Observed=vote_shares_euc1$real_vote_shares, cccl_Euc1=vote_shares_euc1$predicted_vote_shares,cccl_Euc2=vote_shares_euc2$predicted_vote_shares,
                    cccl_Sc1=vote_shares_sc1$predicted_vote_shares,cccl_Sc2=vote_shares_sc2$predicted_vote_shares,
                    cl_Euc=vote_shares_cl_euc$predicted_vote_shares,cl_Sc=vote_shares_cl_sc$predicted_vote_shares)
table_pred[,-1] <- 100*round(table_pred[,-1],3)
xtable(table_pred)


# Draw histograms with distributions of effective number of parties
choice <- cbind(data_cccl_lv_euc2[,c("data.vid","data.choice")])
choice$party <- parties_vect
colnames(choice) <- c("id","choice","party")

library(ggplot2)
    
eff_num_euc2 <- calc_eff_num_of_parties(choice,probs_euc2)
plot_histograms(choice, eff_num_euc2)
plot_histograms_rus(choice, eff_num_euc2)

eff_num_sc2 <- calc_eff_num_of_parties(choice,probs_sc2)
plot_histograms(choice, eff_num_sc2)

# Consideration Set Plots

#1.JR, 2.LDPR, 3.CPRF, 4.Yabloko, 5.UR, 6.RightCause



plot_cons_set_id(df_ideologies,ideology_num=1,"Voter Position on the Left-Right Scale", parties='all',par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=2,"West Countries as an Enemy", parties='all',par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=3,"Democracy", parties='all',par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=4,"Socialism", parties='all',par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=5,"Market Economy", parties='all',par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=6,"Putin's Approval", parties='all',par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=7,"Nationalism", parties='all',par_euc2) 

plot_cons_set_id(df_ideologies,ideology_num=1,"Voter Position on the Left-Right Scale", parties=c(3,5),par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=2,"West Countries as an Enemy", parties=c(3,5),par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=3,"Democracy", parties=c(3,5),par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=4,"Socialism", parties=c(3,5),par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=5,"Market Economy", parties=c(3,5),par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=6,"Putin's Approval", parties=c(3,5),par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=7,"Nationalism", parties=c(3,5),par_euc2) 


plot_cons_set_id(df_ideologies,ideology_num=1,"Voter Position on the Left-Right Scale", parties=c(1,2,3,4),par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=2,"West Countries as an Enemy", parties=c(1,2,3,4),par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=3,"Democracy", parties=c(1,2,3,4),par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=4,"Socialism", parties=c(1,2,3,4),par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=5,"Market Economy", parties=c(1,2,3,4),par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=6,"Putin's Approval", parties=c(1,2,3,4,5),par_euc2) 
plot_cons_set_id(df_ideologies,ideology_num=7,"Nationalism", parties=c(1,2,3,4),par_euc2) 


#--------------------------------------------------------------------------------------

source("/home/anastasia/Documents/Disser/code/disser_functions.R") 

df_ideologies$party <- parties_vect

plot_cons_set_by_party(df_ideologies,ideology_num=1,"Voter Position on the Left-Right Scale", party_code=4, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,3,4),c(1,2,3,4,6),c(4,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=2,"West Countries as an Enemy", party_code=4, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,3,4),c(1,2,3,4,6),c(4,3)),par_euc2) 
plot_cons_set_by_party_color(df_ideologies,ideology_num=3,"Демократия", party_code=1, cons_sets=list(c(1,2,3),c(1,2,3,4),c(1,2,3,4,5,6)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=4,"Socialism", party_code=4, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,3,4),c(1,2,3,4,6),c(4,3)),par_euc2) 
plot_cons_set_by_party_color(df_ideologies,ideology_num=5,"Рыночная экономика", party_code=1, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=6,"Putin's Approval", party_code=4, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,3,4),c(1,2,3,4,6),c(4,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=7,"Nationalism", party_code=4, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,3,4),c(1,2,3,4,6),c(4,3)),par_euc2) 


plot_cons_set_by_party(df_ideologies,ideology_num=1,"Voter Position on the Left-Right Scale", party_code=3, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=2,"West Countries as an Enemy", party_code=3, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=3,"Democracy", party_code=3, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=4,"Socialism", party_code=3, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=5,"Market Economy", party_code=3, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=6,"Putin's Approval", party_code=3, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=7,"Nationalism", party_code=3, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 


plot_cons_set_by_party(df_ideologies,ideology_num=1,"Voter Position on the Left-Right Scale", party_code=1, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=2,"West Countries as an Enemy", party_code=1, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=3,"Democracy", party_code=1, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=4,"Socialism", party_code=1, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=5,"Market Economy", party_code=1, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=6,"Putin's Approval", party_code=1, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=7,"Nationalism", party_code=1, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 


plot_cons_set_by_party(df_ideologies,ideology_num=1,"Voter Position on the Left-Right Scale", party_code=2, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=2,"West Countries as an Enemy", party_code=2, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=3,"Democracy", party_code=2, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=4,"Socialism", party_code=2, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=5,"Market Economy", party_code=2, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=6,"Putin's Approval", party_code=2, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=7,"Nationalism", party_code=2, cons_sets=list(c(1,2,3,4,5,6),c(1,2,3,4),c(1,2,3)),par_euc2) 


plot_cons_set_by_party(df_ideologies,ideology_num=1,"Voter Position on the Left-Right Scale", party_code=5, cons_sets=list(c(1,2,3,4,5,6),c(1,5),c(2,5),c(3,5),c(4,5),c(6,5)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=2,"West Countries as an Enemy", party_code=5, cons_sets=list(c(1,2,3,4,5,6),c(1,5),c(2,5),c(3,5),c(4,5),c(6,5)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=3,"Democracy", party_code=5, cons_sets=list(c(1,2,3,4,5,6),c(1,5),c(2,5),c(3,5),c(4,5),c(6,5)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=4,"Socialism", party_code=5, cons_sets=list(c(1,2,3,4,5,6),c(1,5),c(2,5),c(3,5),c(4,5),c(6,5)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=5,"Market Economy", party_code=5, cons_sets=list(c(1,2,3,4,5,6),c(1,5),c(2,5),c(3,5),c(4,5),c(6,5)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=6,"Putin's Approval", party_code=5, cons_sets=list(c(1,2,3,4,5,6),c(1,5),c(2,5),c(3,5),c(4,5),c(6,5)),par_euc2) 
plot_cons_set_by_party(df_ideologies,ideology_num=7,"Nationalism", party_code=5, cons_sets=list(c(1,2,3,4,5,6),c(1,5),c(2,5),c(3,5),c(4,5),c(6,5)),par_euc2) 



# Probabilities for each party, if voter cast it's ballot for party x

df_probs <- cbind(choice, probs_euc2)

plot_boxplots(df_probs, party="JR")
plot_boxplots(df_probs, party="LDPR")
plot_boxplots(df_probs, party="CPRF")
plot_boxplots(df_probs, party="Yabloko")
plot_boxplots(df_probs, party="UR")
plot_boxplots(df_probs, party="RC")

# Effective Number of Parties in individual voters' effective choice sets depending on main source of information
library(reshape2)
media <- data_cccl_lv_euc3[,c(1,2,14,15,16)]
media <- media[media$data.choice==1,]
media$data.choice <- NULL
colnames(media) <- c("id","Radio","Newspapers","Internet")
media$TV <- ifelse(media$Radio!=1 & media$Newspapers!=1 & media$Internet!=1 , 1, 0)
colnames(media) <- c("id","Радио","Газеты","Интернет", "ТВ")
media <- melt(media, id="id")
media <- media[order(media$id),]
media <- media[media$value==1,]
media$value <- NULL


media_euc2 <- cbind(media, eff_num = eff_num_euc2$eff_num)

plot_histograms_by_media(media_euc2)
plot_histograms_by_media_rus(media_euc2)

media_sc2 <- cbind(media, eff_num = eff_num_sc2$eff_num)
plot_histograms_by_media(media_sc2)

library(shiny)
runGitHub( "disser_app", "terabitiya")

runGitHub( "PartySim", "terabitiya")

setwd("/home/anastasia/Documents/Disser/application/disser_dash_serv")

shinyApp(ui = ui, server = server)

runApp("/home/anastasia/Documents/Disser/application/disser_dash",host="176.59.102.50",port=5050)

shiny::runApp("https://github.com/terabitiya/PartySim.git")
     
library(rsconnect)
rsconnect::setAccountInfo(name='considerationset',
                          token='C9B032DFD3CCE62E9A075CCA579AA1B8',
                          secret='n3ByK5qMXzdvotfhf1FCmP9lfKyJ+//qvdqu3ixX')

library(devtools)
devtools::install_github("rstudio/rsconnect")


rsconnect::deployApp('https://github.com/terabitiya/disser_app')