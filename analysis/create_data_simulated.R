setwd("/home/anastasia/Documents/Disser/data")
load("output_cccl_lv_euc2.RData")
df_ideologies <- read.csv("df_ideologies.csv")
params <- output_lv_euc2$par

create_sim <- function(df_ideologies,ideology_num,ideology_name,params){
  min_ideology <- min(df_ideologies[,ideology_num+3])
  max_ideology <- max(df_ideologies[,ideology_num+3])
  step <- (max_ideology - min_ideology)/10
  ideologies <- c()
  for (x in 1:11) {
    ideologies <- c(ideologies, min_ideology)
    min_ideology <- min_ideology + step
  }
  
  u=6
  
  data_simulated <- data.frame()
  ideol_mean <- colMeans((df_ideologies[,c(4:10)]-df_ideologies[,c(11:17)])^2)
  lr <- df_ideologies[,c(1,2,3,11)]
  lr$party <- rep(c(1:u),nrow(lr)/u)
  lr <- aggregate(lr,by=list(party=lr$party),mean)
  party_scores <- cbind(lr$LR_party,df_ideologies[c(1:u),c(12:17)])
  resp=1
  for (x in ideologies) {
    diff <- t(t((x - party_scores[,ideology_num])^2))
    id <- 1
    for (y in diff) {
      data_simulated <- rbind(data_simulated, c(resp,id,y,x,ideol_mean))
      id <- id + 1
    }
    resp=resp+1
  }
  data_simulated[,ideology_num+4] <- data_simulated[,3]
  data_simulated <- data_simulated[,-c(3)]
  colnames(data_simulated) <- c("id","party","ideology","lr","west_countries","democracy","socialism","market","putin","nationalism")
  
  return(data_simulated)
}


data_simulated_list <- list()
for (i in c(1:7)){
    data_simulated_list[[i]] <- create_sim(df_ideologies,i,ideology_name,params)  
}

save(data_simulated_list, file="data_sim_list.RData")

a <- data_simulated_list[[1]]
