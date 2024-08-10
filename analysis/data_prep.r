setwd("/home/anastasia/Documents/Disser")

load("2012 data no labels.RData")
data <- x
sapply(data, class)

data <- data[order(data$vid),]
table(data$v85, data$v81)
data$v87

library(dummies)
library(reshape)
library(xtable)
xtable(table(data$v81)/nrow(data)*100)
table(data$v81)

data <- data[!is.na(data$v81),]
a <- table(data$v81)/sum(table(data$v81))*100
sum(a)
table(data$v46)
#filter data
data <- data[data$v81<21, ]
table(data$v81)/nrow(data)

v81_dummy <- dummy(data$v81, sep="_")
v81_dummy <- melt(v81_dummy)
v81_dummy <- v81_dummy[order(v81_dummy$X1, v81_dummy$X2),]

data$v40.100 <- rep(0,nrow(data))
v40_dummy <- melt(data[,c('vid','v40.1', 'v40.2', 'v40.3','v40.4','v40.5','v40.6', 'v40.100')], id='vid')
v40_dummy$variable <-  as.numeric(sub(".*v40.", "",v40_dummy$variable))
v40_dummy <- v40_dummy[order(v40_dummy$vid, v40_dummy$variable),]
v40_dummy$variable <- rep(c(6,4,2,1,7,5,3), nrow(v40_dummy)/7)

df <- cbind(v81_dummy$value, v40_dummy)
colnames(df) <- c('party_vote','vid', 'party_code', 'party_LR')
df$party_LR <- ifelse(df$party_LR>10,NA, df$party_LR)
df$party_LR <- ifelse(is.na(df$party_LR),mean(df$party_LR,na.rm=T), df$party_LR)

resp_LR <- c()
for(a in data$v39){resp_LR <- c(resp_LR, rep(a,7))}
df['resp_LR'] <- resp_LR  
df$resp_LR <- ifelse(df$resp_LR>10,NA, df$resp_LR)
df$resp_LR <- ifelse(is.na(df$resp_LR),mean(df$resp_LR,na.rm=T), df$resp_LR)
###########################

vect <- data[,c('vid','v22','v24','v31.1', 'v31.2',
                'v94.1','v94.2','v95.1','v95.2','v95.3','v95.4','v95.5','v95.6','v95.7','v116','v117')]

vect <- do.call("rbind", replicate(7, vect, simplify = FALSE))
vect <- vect[order(vect$vid),]


vect$v22 <- ifelse(vect$v22==2 | vect$v22==1,1,vect$v22)
vect$v22 <- ifelse(vect$v22==3 | vect$v22>=5,0,vect$v22)
vect$v22 <- ifelse(vect$v22==4 | vect$v22==5,-1,vect$v22)

vect$v24 <- ifelse(vect$v24==2 | vect$v24==1,1,vect$v24)
vect$v24 <- ifelse(vect$v24==3 | vect$v24>=5,0,vect$v24)
vect$v24 <- ifelse(vect$v24==4 | vect$v24==5,-1,vect$v24)

vect$v31.1 <- ifelse(vect$v31.1==2 | vect$v31.1==1,1,vect$v31.1)
vect$v31.1 <- ifelse(vect$v31.1>=5,0,vect$v31.1)
vect$v31.1 <- ifelse(vect$v31.1==4 | vect$v31.1==3,-1,vect$v31.1)

vect$v31.2 <- ifelse(vect$v31.2==2 | vect$v31.2==1,1,vect$v31.2)
vect$v31.2 <- ifelse(vect$v31.2>=5,0,vect$v31.2)
vect$v31.2 <- ifelse(vect$v31.2==4 | vect$v31.2==3,-1,vect$v31.2)

vect$v94.1 <- ifelse(vect$v94.1==2 | vect$v94.1==1,1,vect$v94.1)
vect$v94.1 <- ifelse(vect$v94.1>=5,0,vect$v94.1)
vect$v94.1 <- ifelse(vect$v94.1==4 | vect$v94.1==3,-1,vect$v94.1)

vect$v94.2 <- ifelse(vect$v94.2==2 | vect$v94.2==1,1,vect$v94.2)
vect$v94.2 <- ifelse(vect$v94.2>=5,0,vect$v94.2)
vect$v94.2 <- ifelse(vect$v94.2==4 | vect$v94.2==3,-1,vect$v94.2)

vect$v95.3 <- ifelse(vect$v95.3==2 | vect$v95.3==1,1,vect$v95.3)
vect$v95.3 <- ifelse(vect$v95.3>=5,0,vect$v95.3)
vect$v95.3 <- ifelse(vect$v95.3==4 | vect$v95.3==3,-1,vect$v95.3)

vect$v95.2 <- ifelse(vect$v95.2==2 | vect$v95.2==1,1,vect$v95.2)
vect$v95.2 <- ifelse(vect$v95.2>=5,0,vect$v95.2)
vect$v95.2 <- ifelse(vect$v95.2==4 | vect$v95.2==3,-1,vect$v95.2)

vect$v95.1 <- ifelse(vect$v95.1==2 | vect$v95.1==1,1,vect$v95.1)
vect$v95.1 <- ifelse(vect$v95.1>=5,0,vect$v95.1)
vect$v95.1 <- ifelse(vect$v95.1==4 | vect$v95.1==3,-1,vect$v95.1)

vect$v95.4 <- ifelse(vect$v95.4==2 | vect$v95.4==1,1,vect$v95.4)
vect$v95.4 <- ifelse(vect$v95.4>=5,0,vect$v95.4)
vect$v95.4 <- ifelse(vect$v95.4==4 | vect$v95.4==3,-1,vect$v95.4)

vect$v95.5 <- ifelse(vect$v95.5==2 | vect$v95.5==1,1,vect$v95.5)
vect$v95.5 <- ifelse(vect$v95.5>=5,0,vect$v95.5)
vect$v95.5 <- ifelse(vect$v95.5==4 | vect$v95.5==3,-1,vect$v95.5)

vect$v95.6 <- ifelse(vect$v95.6==2 | vect$v95.6==1,1,vect$v95.6)
vect$v95.6 <- ifelse(vect$v95.6>=5,0,vect$v95.6)
vect$v95.6 <- ifelse(vect$v95.6==4 | vect$v95.6==3,-1,vect$v95.6)

vect$v95.7 <- ifelse(vect$v95.7==2 | vect$v95.7==1,1,vect$v95.7)
vect$v95.7 <- ifelse(vect$v95.7>=5,0,vect$v95.7)
vect$v95.7 <- ifelse(vect$v95.7==4 | vect$v95.7==3,-1,vect$v95.7)


vect$v116 <- ifelse(vect$v116==2 | vect$v116==1,1,vect$v116)
vect$v116 <- ifelse(vect$v116==3 | vect$v116>=5,0,vect$v116)
vect$v116 <- ifelse(vect$v116==4 | vect$v116==5,-1,vect$v116)

vect$v117 <- ifelse(vect$v117==2 | vect$v117==1,1,vect$v117)
vect$v117 <- ifelse(vect$v117==3 | vect$v117>=5,0,vect$v117)
vect$v117 <- ifelse(vect$v117==4 | vect$v117==5,-1,vect$v117)

df <- cbind(df,vect[,-c(1)])

write.csv(df,'prep_data_101.csv')

##########################


vect <- data[,c('vid','v31.1', 
              "v22",'v48.5','v31.2',
              'v94.1','v94.2','v95.1','v95.2','v95.3','v95.4','v95.5','v95.6','v95.7','v116','v117',
              'v97.1','v97.2','v97.3','v97.6','v97.7','v97.8','v97.9')]

v87 <- dummy(data$v87, sep=".")
v90 <- dummy(data$v90, sep=".")

vect <- cbind(vect, v87[,-c(4,5)],v90[,-c(5)])

vect <- do.call("rbind", replicate(7, vect, simplify = FALSE))
vect <- vect[order(vect$vid),]

df['elect_viability'] <- rep(c(0.4932*450, 0.1919*450, 0.1324*450, 0.1167*450, 0.0343*450, 0.006*450, 0.0097*450),nrow(df)/7)
df <- cbind(df,vect[,-c(1)])


df$v22 <- ifelse(df$v22>=2,1,0)
df$v48.5 <- ifelse(df$v48.5>=2,1,0)
df$v31.1 <- ifelse(df$v31.1>=2,1,0)
df$v31.2 <- ifelse(df$v31.2>=2,1,0)
df$v94.1 <- ifelse(df$v94.1>=2,1,0)
df$v94.2 <- ifelse(df$v94.2>=2,1,0)
df$v95.1 <- ifelse(df$v95.1>=2,1,0)
df$v95.2 <- ifelse(df$v95.2>=2,1,0)
df$v95.3 <- ifelse(df$v95.3>=2,1,0)
df$v95.4 <- ifelse(df$v95.4>=2,1,0)
df$v95.5 <- ifelse(df$v95.5>=2,1,0)
df$v95.6 <- ifelse(df$v95.6>=2,1,0)
df$v95.7 <- ifelse(df$v95.7>=2,1,0)
df$v116 <- ifelse(df$v116>=4,1,0)
df$v117 <- ifelse(df$v117>=2,1,0)
df$v97.1 <- ifelse(df$v97.1>=2,1,0)
df$v97.2 <- ifelse(df$v97.2>=2,1,0)
df$v97.3 <- ifelse(df$v97.3>=2,1,0)
df$v97.6 <- ifelse(df$v97.6>=2,1,0)
df$v97.7 <- ifelse(df$v97.7>=2,1,0)
df$v97.8 <- ifelse(df$v97.8>=2,1,0)
df$v97.9 <- ifelse(df$v97.9>=2,1,0)

###################################

vect <- data[,c('vid','v22','v24','v31.1', 'v31.2',
                'v94.1','v94.2','v95.1','v95.2','v95.3','v95.4','v95.5','v95.6','v95.7','v116','v117','v132')]

vect <- do.call("rbind", replicate(7, vect, simplify = FALSE))
vect <- vect[order(vect$vid),]

library(car)

vect[,c("v22", "v24","v116", "v117","v132")] <- apply( vect[,c("v22", "v24","v116", "v117","v132")], 2,function (x) ifelse(x>5, NA, x))
vect[,c("v22", "v24","v116", "v117","v132")] <- apply( vect[,c("v22", "v24","v116", "v117","v132")], 2,function (x) recode(x, "1= 10; 2=7.25; 3=5.5; 4=3.75; 5=1"))
vect[,c("v22", "v24","v116", "v117","v132")] <- apply( vect[,c("v22", "v24","v116", "v117","v132")], 2,function (x) ifelse(is.na(x), mean(x, na.rm=TRUE), x))


vect[,c("v31.1", "v31.2","v94.1", "v94.2", "v95.1", "v95.2", "v95.3", "v95.4", "v95.5", "v95.6", "v95.7")] <- apply( 
  vect[,c("v31.1", "v31.2","v94.1", "v94.2", "v95.1", "v95.2", "v95.3", "v95.4", "v95.5", "v95.6", "v95.7")], 2,function (x) ifelse(x>4, NA, x))
vect[,c("v31.1", "v31.2","v94.1", "v94.2", "v95.1", "v95.2", "v95.3", "v95.4", "v95.5", "v95.6", "v95.7")] <- apply( 
  vect[,c("v31.1", "v31.2","v94.1", "v94.2", "v95.1", "v95.2", "v95.3", "v95.4", "v95.5", "v95.6", "v95.7")], 2,function (x) recode(x, "1= 10; 2=7; 3=4; 4=1"))
vect[,c("v31.1", "v31.2","v94.1", "v94.2", "v95.1", "v95.2", "v95.3", "v95.4", "v95.5", "v95.6", "v95.7")] <- apply(
  vect[,c("v31.1", "v31.2","v94.1", "v94.2", "v95.1", "v95.2", "v95.3", "v95.4", "v95.5", "v95.6", "v95.7")],2, function (x) ifelse(is.na(x), mean(x, na.rm=TRUE), x))





vect[,c("v22", "v24","v116", "v117","v132")] <- apply( vect[,c("v22", "v24","v116", "v117","v132")], 2,function (x) ifelse(x>5, NA, x))
vect[,c("v22", "v24","v116", "v117","v132")] <- apply( vect[,c("v22", "v24","v116", "v117","v132")], 2,function (x) recode(x, "1= 1; 2=3.25; 3=5.5; 4=7.75; 5=10"))
vect[,c("v22", "v24","v116", "v117","v132")] <- apply( vect[,c("v22", "v24","v116", "v117","v132")], 2,function (x) ifelse(is.na(x), mean(x, na.rm=TRUE), x))


vect[,c("v31.1", "v31.2","v94.1", "v94.2", "v95.1", "v95.2", "v95.3", "v95.4", "v95.5", "v95.6", "v95.7")] <- apply( 
  vect[,c("v31.1", "v31.2","v94.1", "v94.2", "v95.1", "v95.2", "v95.3", "v95.4", "v95.5", "v95.6", "v95.7")], 2,function (x) ifelse(x>4, NA, x))
vect[,c("v31.1", "v31.2","v94.1", "v94.2", "v95.1", "v95.2", "v95.3", "v95.4", "v95.5", "v95.6", "v95.7")] <- apply( 
  vect[,c("v31.1", "v31.2","v94.1", "v94.2", "v95.1", "v95.2", "v95.3", "v95.4", "v95.5", "v95.6", "v95.7")], 2,function (x) recode(x, "1= 1; 2=4; 3=7; 4=10"))
vect[,c("v31.1", "v31.2","v94.1", "v94.2", "v95.1", "v95.2", "v95.3", "v95.4", "v95.5", "v95.6", "v95.7")] <- apply(
  vect[,c("v31.1", "v31.2","v94.1", "v94.2", "v95.1", "v95.2", "v95.3", "v95.4", "v95.5", "v95.6", "v95.7")],2, function (x) ifelse(is.na(x), mean(x, na.rm=TRUE), x))

describe(vect)

df['elect_viability'] <- rep(c(0.4932*450, 0.1919*450, 0.1324*450, 0.1167*450, 0.0343*450, 0.006*450, 0.0097*450),nrow(df)/7)

df <- cbind(df,vect[,-c(1)])

write.csv(df,'prep_data_int.csv')
write.csv(df,'prep_data_int_reversed.csv')
############################
# FACTOR ANALYSIS (ZAKHAROV)
############################
#df <- read.csv("prep_data_101.csv")
df <- read.csv('prep_data_int.csv')

df_factors <- df[df$party_vote==1,]
colnames(df_factors)

985+985-30

df_factors <- df_factors[,-c(1,3:6,22)]
library(psych)
#########
poly_cor <-  polychoric(df_factors[,-c(1)])
poly_model = fa(df_factors[,-c(1)], nfactor=5, cor="poly", fm="mle", rotate = "varimax")
poly_model$loadings
###########
fact.res <- factanal(df_factors[,-c(1)], 5, rotation="varimax")
print(fact.res)
  fact.res
##########

#library(psych)
#fact.res <- fa.poly(df_factors[,-c(1,2,3,4,5,6)], nfactors=4, rotate="varimax")
#print(fact.res)
fact_scores <- t(rbind(fact.res$loadings[,1] %*% t(df_factors[,-c(1)]),
                     fact.res$loadings[,2] %*% t(df_factors[,-c(1)]),
                     fact.res$loadings[,3] %*% t(df_factors[,-c(1)]),
                     fact.res$loadings[,4] %*% t(df_factors[,-c(1)]),
                     fact.res$loadings[,5] %*% t(df_factors[,-c(1)])))

fact_scores <- as.data.frame(fact_scores)
fact_scores$vid <- df_factors$vid
fact_scores <- do.call("rbind", replicate(7, fact_scores, simplify = FALSE))
fact_scores <- fact_scores[order(fact_scores$vid),]

df <- cbind(df,fact_scores)
df <- df[order(df$vid,df$party_code),]

write.csv(df,"fact_scores.csv", row.names=F)

party_scores <- df[df$party_vote==1,]
party_scores <- aggregate(party_scores, by=list(party_scores$party_code), mean)[,c("V1","V2","V3","V4","V5")]
write.csv(party_scores,"party_scores.csv", row.names=F)
party_scores <-do.call("rbind", replicate(nrow(df)/7, party_scores, simplify = FALSE))
#write.csv(df,"party_scores.csv", row.names=F)

#EUCLIDIAN DISTANCE
dist_fact <-  (df[,c("V1","V2","V3","V4","V5")]- party_scores)^2

(df$V1 - party_scores$V1)^2
#SCALAR PRODUCT
Nm <- 0
scal_fact <-(df[,c("V1","V2","V3","V4","V5")]-Nm)*(party_scores - Nm)

df[1,c("V1","V2","V3","V4","V5")]
party_scores[1,]
scal_fact[1,]


########################
# MULTINOMIAL LOGIT
data_mln <- cbind(df[,c(2,1,3,4,5,6)],dist_fact)
dem <- data[,c("vid","man","age","educ3","v127")]
dem$educ3 <- as.factor(dem$educ3)
dem$v127 <- as.factor(dem$v127)
dem <- do.call("rbind", replicate(7, dem, simplify = FALSE))
dem <- dem[order(dem$vid),]
data_mln <- cbind(data_mln,dem[,-c(1)])
data_mln$party_code <- as.factor(data_mln$party_code)
data_mln[,c("age", "V1","V2","V3","V4","V5")] <- scale(data_mln[,c("age", "V1","V2","V3","V4","V5")], center = TRUE, scale = TRUE)

require(nnet)
mln <- multinom(party_code~V1 +V2+V3+V4+V5+man+age+educ3+v127,data=data_mln)
summary(mln)

library(stargazer)
stargazer(mln, type="html")

colnames(data_mln)
#########################

#########################
#CCCL

Nm=0
pol_ext <- apply(party_scores,2, function(x) (x-Nm)^2)
pol_ext <- sqrt(rowSums(pol_ext))

table(data$v42)
table(data$v44)

sa <- data
sa$vote_sa <- 0
sa$v44 <- ifelse(is.na(sa$v44), 99,sa$v44)
sa[sa$v42==1 & sa$v44==1,"vote_sa"] <- 1
sum(sa$vote_sa)

sa[sa$vote_sa==1, "vote_sa"] <-  sa[sa$vote_sa==1, "v81"]
sa <- sa[,c("vid", "vote_sa")]


strong_aff <- c()
for (i in sa$vote_sa){ strong_aff <- c(strong_aff,rep(i,7))}


df$strong_aff <-0
df[strong_aff==1 , "strong_aff"] <- df[strong_aff==1, "party_vote"] 



#to test the model performance  
df$strong_aff[which(df$strong_aff==0)] <- df$strong_aff[which(df$strong_aff==0)]  + rbinom(length(df$strong_aff[which(df$strong_aff==0)]), 1, 0.1)

data_cccl <- cbind(df$vid, df$party_vote, dist_fact$V1, dist_fact$V2, dist_fact$V3, dist_fact$V4, dist_fact$V5, 1, 
                   df$elect_viability,V10 = pol_ext, df$strong_aff)

write.csv(data_cccl, "data_cccl.csv", row.names=FALSE)


data_cccl <- read.csv("data_cccl.csv")
#data_cccl$X.8 <-data_cccl$X.8/450
data_cccl <- as.matrix(data_cccl)


data_cccl[,c(9)] <- scale(data_cccl[,c(9)])
#data_cccl[,c(9,10)] <- scale(data_cccl[,c(9,10)])

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


#use optim function to get the estimates
output <- optim(rep(1, times = 9), logit.cccl, method = 'BFGS', hessian = TRUE, data=data_cccl, division = 5)
#test whether converged or not
output$convergence == 0
#check whether parameters are the same as in the paper - they mostly are, except for some very minor differences
output$par
#get the standard errors - also very similar; we can use them to test hypotheses and construct confidence intervals
t_hessian <- solve(output$hessian)
se <- sqrt(diag(t_hessian))
se  
#work with oarty_scores
min_ideology <- min(fact_scores$V1)
max_ideology <- max(fact_scores$V1)
step <- (max_ideology - min_ideology)/10
ideologies <- c()
for (x in 1:11) {
  ideologies <- c(ideologies, min_ideology)
  min_ideology <- min_ideology + step
}
data_simulated <- data.frame()
for (x in ideologies) {
  differences_V1 <- t(t((x - party_scores$V1)^2))
  id <- 1
  for (y in differences_V1) {
    data_simulated <- rbind(data_simulated, c(id, x, y, mean(data_cccl[,4]), mean(data_cccl[,5]), mean(data_cccl[,6]), mean(data_cccl[,7])))
    id <- id + 1
  }
}
data_KPRF_UR <- data_simulated[which((data_simulated$X1 == 4) | (data_simulated$X1 == 6)),]
data_KPRF_UR$pr_scores <- exp(as.matrix(data_KPRF_UR[,3:7])%*%output$par[1:5])
colnames(data_KPRF_UR)[2] <- "Putinism"

aggr_sum <- aggregate(data_KPRF_UR$pr_scores, by=list(data_KPRF_UR[,2]), FUN = sum)
colnames(aggr_sum)[1] <- "Putinism"
data_KPRF_UR_plot <- merge(data_KPRF_UR, aggr_sum, by = "Putinism")
data_KPRF_UR_plot$final_probs <- data_KPRF_UR_plot$pr_scores/data_KPRF_UR_plot$V1
plot <- ggplot(data_KPRF_UR_plot[which(data_KPRF_UR_plot$X1 == 4),]) +
  geom_line(aes(y = final_probs, x = Putinism))

for (x in party_scores$V1) {
  data.fr
}
fact_scores$V1


output$par/se

##################
# LATENT VARIABLES
##################

setwd("/home/anastasia/Documents/Disser")
library(openxlsx)
df_lv <- read.csv("df_for_latent_vars.csv")


df_manifesto <- read.xlsx("Manifesto (copy).xlsx")
df_manifesto_rus <- df_manifesto[df_manifesto$countryname=="Russia" & df_manifesto$coderyear==2011,]
df_manifesto_rus <- df_manifesto_rus[-c(5,6,7),]


west_countries <- df_manifesto_rus$per108 + df_manifesto_rus$per110
democracy_freedom <- df_manifesto_rus$per102+df_manifesto_rus$per202+df_manifesto_rus$per301
big_government <- df_manifesto_rus$per403+df_manifesto_rus$per402+df_manifesto_rus$per404+df_manifesto_rus$per405+
                  df_manifesto_rus$per406+df_manifesto_rus$per409+df_manifesto_rus$per412+df_manifesto_rus$per413+df_manifesto_rus$per415
small_government <- df_manifesto_rus$per401+df_manifesto_rus$per402+df_manifesto_rus$per414
crisis <- df_manifesto_rus$per6061

west_countries <- west_countries/sum(west_countries)
democracy_freedom <- democracy_freedom/sum(democracy_freedom)
big_government <- big_government/sum(big_government)
small_government <- small_government/sum(small_government)
crisis <- crisis/sum(crisis)

vars_manifesto <- as.data.frame(cbind(democracy_freedom, big_government, small_government,crisis))
vars_manifesto$sort <-c(4,2,1,6,7,5,3) 
vars_manifesto <- vars_manifesto[order(vars_manifesto$sort),]
vars_manifesto$sort <- NULL

vars_manifesto <- do.call("rbind", replicate(nrow(df_lv)/7, vars_manifesto, simplify = FALSE))

df_lv <- cbind(df_lv, vars_manifesto)

# democracy_free: V31.1, v95.1, v95.5, v116
# socialism and regulation: v95.3, v87.1
# free market: v95.4
# crisis: v132

library(runjags)
library(rjags)
library(MASS)
library(parallel)
library(R2jags)


#################

setwd("/home/anastasia/Documents/Disser")

load("2012 data no labels.RData")
data <- x

data <- data[order(data$vid),]
data <- data[!is.na(data$v81),]
data <- data[data$v81<21, ]


lv_model1 <- "model{
for(i in 1:N){
  for(j in 1:NDV){
    pi[i,j]<- alpha[j]+ beta[j]*xi[i]
    y[i,j] ~ dordered.probit(pi[i,j],tau[j,1:3])
  }
}

for(i in 1:NDV){
  taustar[i,1:3] ~ dmnorm(t0,T0)
  tau[i,1:3] <- sort(taustar[i,1:3])
  beta[i] ~ dnorm(0,.1)T(0,)
  alpha[i] ~dnorm(0,.1)
}


for(i in 1:N){
  xi[i] ~ dnorm(0,1)
}
}"

lv_model1 <- function(){
for(i in 1:N){
  for(j in 1:NDV){
    pi[i,j]<- alpha[j]+ beta[j]*xi[i]
    y[i,j] ~ dordered.probit(pi[i,j],tau[j,1:3])
  }
}

for(i in 1:NDV){
  taustar[i,1:3] ~ dmnorm(t0,T0)
  tau[i,1:3] <- sort(taustar[i,1:3])
  beta[i] ~ dnorm(0,.1)T(0,)
  alpha[i] ~dnorm(0,.1)
}


for(i in 1:N){
  xi[i] ~ dnorm(0,1)
}
}


# democracy_free
y=data[,c("v31.1","v95.1","v116")]
y[y==7 | y==8 | y==9] = NA
y = as.data.frame(apply(y,2, function(x) ifelse(is.na(x),1,x)))

hmod<-list(N=nrow(data), NDV=2, t0=rep(0,3),T0=diag(.1,3), y=as.matrix(y))

taustar=rbind(c(qnorm(mean(y$v31.1<2,na.rm=T)),qnorm(mean(y$v31.1<3,na.rm=T)),qnorm(mean(y$v31.1<4,na.rm=T))),
              c(qnorm(mean(y$v95.1<2,na.rm=T)),qnorm(mean(y$v95.1<3,na.rm=T)),qnorm(mean(y$v95.1<4,na.rm=T))))
taustar3 = c(qnorm(mean(y$v116<2,na.rm=T)),qnorm(mean(y$v116<3,na.rm=T)),qnorm(mean(y$v116<4,na.rm=T)),qnorm(mean(y$v116<5,na.rm=T)))

hmodel <- run.jags(method="parallel",model=lv_model, monitor=c("alpha","beta","tau","xi"), 
                   inits=list(taustar=taustar), data=hmod, n.chains=1, burnin=10000, 
                   sample=1250, thin=80, summarise=F,plots=F, modules=c("glm","lecuyer"))


cds<-as.mcmc.list(hmodel)
cds<-as.mcmc(hmodel)
plot(cds)

# socialism and regulation

lv_model2 <- "model{
for(i in 1:N){
    pi1[i]<- alpha1+ beta1*xi[i]
    y[i,1] ~ dordered.probit(pi1[i],tau[1:3])

}

for(i in 1:N){
    probit(pi2[i]) <- alpha2+ beta2*xi[i]  
    y[i,2] ~ dbern(pi2[i])
}

  alpha1 ~ dnorm(0,.1)
  beta1 ~ dnorm(0,.1)T(0,)
  alpha2 ~ dnorm(0,.1)
  beta2 ~ dnorm(0,.1)T(0,)
    
  taustar[1:3] ~ dmnorm(t0,T0)
  tau[1:3] <- sort(taustar[1:3])


for(i in 1:N){
  xi[i] ~ dnorm(0,1)
}
}"

lv_model2 <- function(){
    for (i in 1:N){
      pi1[i]<- alpha1 + beta1*xi[i]
      y[i,1] ~ dordered.probit(pi1[i],tau1[1:3])
  
  }
for (i in 1:N){
    pi2[i] <- alpha2 + beta2*xi[i]  
    y[i,2] ~ dordered.probit(pi2[i],tau2[1:3])
}
  alpha1 ~ dnorm(0,.1)
  beta1 ~ dunif(0,1)
  alpha2 ~ dnorm(0,.1)
  beta2 ~ dunif(0,1)
    
  taustar1[1:3] ~ dmnorm(t0,T0)
  tau1[1:3] <- sort(taustar1[1:3])
  taustar2[1:3] ~ dmnorm(t0,T0)
  tau2[1:3] <- sort(taustar2[1:3])

for (i in 1:N){
  xi[i] ~ dnorm(0,1)
}
}

x1 <- rbinom(100, 3, 0.7)
x2 <- rbinom(100, 3, 0.7)
y <- cbind(x1,x2)
hmod<-list(N=nrow(y), t0=rep(0,3), T0=diag(.1,3), y=as.matrix(y))

y=data[,c("v95.3","v87")]
y$v87 <- ifelse(y$v87==1,1,0)
y[y==7 | y==8 | y==9] = NA
y = as.data.frame(apply(y,2, function(x) ifelse(is.na(x),1,x)))
#y$v95.3 = y$v95.3 - 1

hmod<-list(N=nrow(data), t0=rep(0,3),T0=diag(.1,3), y=as.matrix(y))

taustar=c(qnorm(mean(y$v95.3<2,na.rm=T)),qnorm(mean(y$v95.3<3,na.rm=T)),qnorm(mean(y$v95.3<4,na.rm=T)))

params = c("alpha1","beta1","alpha2","beta2","xi")
hmodel <- jags(data = hmod, inits = NULL, params, n.chains = 3, n.iter = 1000, model.file = lv_model2)

hmodel <- run.jags(method="parallel",model=lv_model2, monitor=c("alpha1","beta1","alpha2","beta2","xi"), data=hmod, n.chains=1, burnin=10000, 
                   sample=1250, thin=80, summarise=F,plots=F, modules=c("glm","lecuyer"))
failed.jags()

cds<-as.mcmc.list(hmodel)
cds<-as.mcmc(hmodel)






##########################################################################
# Model with media index, left-right, what source of information respondent considers the main one 
##########################################################################
df <- read.csv('prep_data_int_reversed.csv')
data_cccl <- read.csv("data_cccl.csv")

sa <- data
sa$vote_sa <- 0
sa$v44 <- ifelse(is.na(sa$v44), 99,sa$v44)
sa[sa$v42==1,"vote_sa"] <- 1
sum(sa$vote_sa)

sa[sa$vote_sa==1, "vote_sa"] <-  sa[sa$vote_sa==1, "v81"]
sa <- sa[,c("vid", "vote_sa")]

strong_aff <- c()
for (i in sa$vote_sa){ strong_aff <- c(strong_aff,rep(i,7))}

data_cccl[,11] <-0
data_cccl[strong_aff==1 , 11] <- data_cccl[strong_aff==1, 2] 
sum(data_cccl[,11])

df$party_LR <- ifelse(df$party_LR>10,NA,df$party_LR)
df$party_LR <- ifelse(is.na(df$party_LR),mean(df$party_LR,na.rm=T),df$party_LR)

df$resp_LR <- ifelse(df$resp_LR>10,NA,df$resp_LR)
df$resp_LR <- ifelse(is.na(df$resp_LR),mean(df$resp_LR,na.rm=T),df$resp_LR)

# EUCLIDIAN DISTANCE
df$LR <- (df$resp_LR-df$party_LR)^2

data_cccl <- cbind(data_cccl[,c(1,2)],df$LR, data_cccl[,-c(1,2)])
data_cccl[,c(3,10,11)] <- scale(data_cccl[,c(3,10,11)])

v18 <- data[,c("v18.1", "v18.2", "v18.3","v18.4")]
colSums(!is.na(v18))
v18[!is.na(v18)] <- 1
v18[is.na(v18)] <- 0

# add v18 without v18.1 (TV)

data_cccl <- cbind(data_cccl,v18[,-c(1)])

data_cccl <- as.matrix(data_cccl)

logit.cccl <- function(beta, data, division) {
  betas <- data[,3:(division + 2)]%*%beta[1:division]
  gammas <- -data[,(division+3):(length(beta)+2)]%*%beta[(division+1):length(beta)]
  second_term <- log(1 + exp(gammas))
  third_term <- exp(betas)*(1/(1 + exp(gammas)))
  data_1 <- data.frame('id' = data[,1], 'y' = data[,2], first_half = betas - second_term)
  data_2 <- data.frame('id' = data[,1], 'y' = data[,2], 'to_sum' = third_term)
  to_merge <- aggregate(data_2$to_sum, by = list(id = data_2$id), FUN = sum)
  data_1 <- merge(data_1, to_merge, by=c('id'))
  return(-sum((data_1[,'first_half'] - log(data_1[,'x']))*data_1[,'y']))
}

sum(data_cccl[,12])

output <- optim(rep(1, times = 9+3+1), logit.cccl, method = 'BFGS', hessian = TRUE, data=data_cccl, division = 6)
output$convergence == 0
output$par
t_hessian <- solve(output$hessian)
se <- sqrt(diag(t_hessian))
output$par/se  

# how the change in the source of information influences the probability of choosing a certain party
fact_scores <- read.csv("fact_scores.csv")
party_scores <- read.csv("party_scores.csv")

min_ideology <- min(fact_scores$V1)
max_ideology <- max(fact_scores$V1)
step <- (max_ideology - min_ideology)/10
ideologies <- c()
for (x in 1:11) {
  ideologies <- c(ideologies, min_ideology)
  min_ideology <- min_ideology + step
}

x=ideologies[1]
data_simulated <- data.frame()
for (x in ideologies) {
  differences_V1 <- t(t((x - party_scores$V1)^2))
  id <- 1
  for (y in differences_V1) {
    data_simulated <- rbind(data_simulated, c(id, x, mean(data_cccl[,3]), y, mean(data_cccl[,5]), mean(data_cccl[,6]), mean(data_cccl[,7]), mean(data_cccl[,8])))
    id <- id + 1
  }
}


data_simulated$pr_scores <-  exp(as.matrix(data_simulated[,3:8])%*%output$par[1:6])
colnames(data_simulated)[2] <- "Anti_Putinism"

data_sim_tv <- cbind(rep(1,77), rep(mean(data_cccl[,10]),77), rep(mean(data_cccl[,11]),77), rep(0,77), rep(0,77), rep(0,77), rep(0,77))
data_sim_tv <- cbind(data_sim_tv, 1/(1+exp(-data_sim_tv%*%output$par[7:13])))


aggr_sum <- aggregate(data_simulated$pr_scores, by=list(data_simulated[,2]), FUN = sum)
colnames(aggr_sum)[1] <- "Anti_Putinism"
data_simulated_plot <- merge(data_simulated, aggr_sum, by = "Anti_Putinism")
data_simulated_plot$final_probs <- data_simulated_plot$pr_scores/data_simulated_plot$V1

# Plot
plot <- ggplot(data_simulated_plot[which(data_simulated_plot$X1 == 6),]) +
  geom_line(aes(y = final_probs, x = Anti_Putinism))
plot



######
penp <- 1/(1+exp(-as.matrix(data_cccl[,c(10:15)])%*%output$par[8:13]))
penp <- cbind(data_cccl[,c(1,2)],penp)
aggr <- aggregate(penp[,3], by=list(penp[,1]), FUN = sum)
colnames(aggr)[1] <-"X" 
penp_plot <- merge(penp, aggr, by = "X")
penp_plot[,3] <- (penp_plot[,3]/penp_plot[,4])^2
penp_plot <- aggregate(penp_plot[,3], by=list(penp_plot[,1]), FUN = sum)
penp_plot$x <- 1/penp_plot$x
hist(penp_plot$x)
  



