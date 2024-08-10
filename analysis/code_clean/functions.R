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

calc_prob_cccl <- function(df, params, division){
  id <- df[,1]
  df <- df[,-c(1,2)]
  cons_set <- 1/(1+exp(-as.matrix(df[,-c(1:division)])%*%params[(division+1):length(params)])) 
  term2 <- exp(as.matrix(df[,-c((division+1):ncol(df))])%*%params[1:division])
  prod <- cons_set*term2
  df_calc <- as.data.frame(cbind(id=id,prod))
  aggr <- aggregate(df_calc$V2, by=list(df_calc$id), sum)
  colnames(aggr) <- c("id","prod_sum")
  df_calc <- merge(df_calc,aggr, by=c('id'))
  probs <- df_calc[,2]/df_calc[,3]
  return(probs)  } 



calc_vote_shares <- function(df){
  colnames(df) <- c("id","choice","party","probs")
  aggr <- aggregate(df$probs, by=list(df$id), FUN=max)
  colnames(aggr) <- c("id","probs_max")
  df <- merge(df,aggr, by="id")
  df$predicted_vote <- as.integer(df$probs == df$probs_max)
  vote_shares <- aggregate(df[,c(2,6)], by=list(df$party),FUN=sum)
  vote_shares$perc_real <-vote_shares$choice/sum(vote_shares$choice)
  vote_shares$perc_pred <- vote_shares$predicted_vote/sum(vote_shares$predicted_vote)
  colnames(vote_shares) <- c("party","choice","predicted_choice","real_vote_shares","predicted_vote_shares")
  return(vote_shares)
}

calc_eff_num_of_parties <- function(choice,probs){
  df <- choice
  df$probs <- probs
  df$probs_sq <-df$probs^2
  penp <- aggregate(df$probs_sq, by=list(df$id), FUN=sum)
  colnames(penp) <- c("id","eff_num")
  penp$eff_num <- 1/penp$eff_num
  return(penp)
}

plot_histograms <- function(choice, eff_num){
  df <- merge(choice,eff_num, by="id")
  df <- df[df$choice==1,]
 plot= ggplot(df, aes(eff_num))+geom_histogram(aes(y=..density..),color="gray",binwidth = 0.35)+
   facet_wrap(~party)+xlab("Effective Number of Parties in Individual Voters' Effective Choice Sets")+theme_bw()
  return(plot)
  }

plot_histograms_rus <- function(choice, eff_num){
  df <- merge(choice,eff_num, by="id")
  df <- df[df$choice==1,]
  plot= ggplot(df, aes(eff_num))+geom_histogram(aes(y=..density..),color="gray",binwidth = 0.35)+
    facet_wrap(~party)+xlab("Эффективное число партий в индивидуальном consideration set")+theme_bw()
  return(plot)
}

plot_cons_set_id <- function(df_ideologies,ideology_num,ideology_name,parties="all",params){
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

  if(parties=="all"){
    data_simulated$pr_scores <- exp(as.matrix(data_simulated[,5:11])%*%params[1:7])
    colnames(data_simulated)[1] <- "id"
    aggr_sum <- aggregate(data_simulated$pr_scores, by=list(data_simulated[,1]), FUN = sum)
    colnames(aggr_sum)[1] <- "id"
    data_plot <- merge(data_simulated, aggr_sum, by = "id")
    colnames(data_plot)[2] <- "party"
    data_plot$party <- rep(as.character(df_ideologies$party[1:u]),nrow(data_plot)/u)
    colnames(data_plot)[4] <- "ideology"
    data_plot$Pr <- data_plot$pr_scores/data_plot$V1
    data_plot$party <- as.factor(data_plot$party)
    plot <- ggplot(data=data_plot, aes(x=ideology, y=Pr,group=party))+
      geom_line(aes(linetype=party))+
      geom_point(aes(shape=party))+
      theme(legend.position="right")+
      xlab(ideology_name)+
      theme_bw()
  }
  
  else{
    filt <- rep(FALSE,nrow(data_simulated)/u)
    p_names <- c()
    for (i in parties){
      filt <- rep(c(1:u)==i, nrow(data_simulated)/u)+filt
      p_names <- c(p_names, as.vector(df_ideologies$party)[i])
    }
   
    data_simulated <- data_simulated[as.logical(filt),]
    
    data_simulated$pr_scores <- exp(as.matrix(data_simulated[,5:11])%*%params[1:7])
    colnames(data_simulated)[1] <- "id"
    aggr_sum <- aggregate(data_simulated$pr_scores, by=list(data_simulated[,1]), FUN = sum)
    colnames(aggr_sum)[1] <- "id"
    data_plot <- merge(data_simulated, aggr_sum, by = "id")
    colnames(data_plot)[2] <- "party"
    data_plot$party <- rep(p_names,nrow(data_plot)/length(p_names))
    colnames(data_plot)[4] <- "ideology"
    data_plot$Pr <- data_plot$pr_scores/data_plot$V1
    data_plot$party <- as.factor(data_plot$party)
    plot <- ggplot(data=data_plot, aes(x=ideology, y=Pr,group=party))+
      geom_line(aes(linetype=party))+
      geom_point(aes(shape=party))+
      theme(legend.position="right")+
      xlab(ideology_name)+
      theme_bw()
  }
  
  return(plot)
}


plot_histograms_by_media <- function(media){
  plot= ggplot(media, aes(eff_num))+geom_histogram(aes(y=..density..),color="gray",binwidth = 0.35)+
    facet_wrap(~variable)+xlab("Effective Number of Parties in Individual Voters' Effective Choice Sets")+theme_bw()
  return(plot)
    }  

plot_histograms_by_media_rus <- function(media){
  plot= ggplot(media, aes(eff_num))+geom_histogram(aes(y=..density..),color="gray",binwidth = 0.35)+
    facet_wrap(~variable)+xlab("Эффективное число партий в индивидуальном consideration set")+theme_bw()
  return(plot)
}         
 
make_table <- function(output, k,obs,n){
  table <- data.frame(matrix(0,ncol=1,nrow=n))
  log_lik <- -output$value
  aic=round(2*k-2*log_lik,3)
  bic=round(log(obs)*k-2*log_lik,3)
  m=n-k*2-4
  
  pars <- round(output$par,3)
  t_hessian <- solve(output$hessian)
  
  se <- sqrt(diag(t_hessian))
  z <- output$par/se 
  se <- round(se,3)
  coef <- c()
  
  for (i in c(1:length(pars))){coef <- c(coef, as.character(pars[i]), paste0("(",as.character(se[i]),")"))}
  for (i in (1:length(z))){
    p_value = 2*pnorm(-abs(z[i])) 
    if (p_value<0.001){
      coef[i*2-1]=paste0(coef[i*2-1],"***")
    }
    else if (p_value<=0.01 & p_value >0.001){
      coef[i*2-1]=paste0(coef[i*2-1],"**")
    }
    else if (p_value<=0.05 & p_value>0.01){
      coef[i*2-1]=paste0(coef[i*2-1],"*")
    }
  }
  
  table$coef <- c(coef,rep("",m),round(log_lik,3),aic,bic,obs)
  return(table)
}



conditional.logit <- function(beta,data){
  lp <- exp(data[,3:ncol(data)]%*%beta)
  aggr <- cbind(data[,1],lp)
  aggr <- aggregate(aggr[,-1], by=list(aggr[,1]), FUN=sum)
  colnames(aggr)[1] <-"id" 
  data1 <- data.frame('id' = data[,1], 'y' = data[,2], lp = lp)
  data1 <- merge(data1,aggr, by=c("id"))
  cond.logit <- data1$lp/data1$x
  ll <- sum(log(cond.logit)*data1$y)
  return(-ll)
}


calc_prob_cl <- function(df,params){
  lp <- exp(as.matrix(df[,3:ncol(df)])%*%params)
  aggr <- cbind(df[,1],lp)
  aggr <- aggregate(aggr[,-1], by=list(aggr[,1]), FUN=sum)
  colnames(aggr)[1] <-"id" 
  data1 <- data.frame('id' = df[,1], 'y' = df[,2], lp = lp)
  data1 <- merge(data1,aggr, by=c("id"))
  probs <- data1$lp/data1$x
  return(probs)  
}



plot_cons_set_by_party <- function(df_ideologies,ideology_num,ideology_name,party_code,cons_sets,params){
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
  parties <- as.vector(df_ideologies$party)[1:6]
  
  data_plot_all <- data.frame()
  for (cons_set in cons_sets){
    filt <- rep(FALSE,nrow(data_simulated)/u)
    p_names <- c()
    for (i in unlist(cons_set)){
      filt <- rep(c(1:u)==i, nrow(data_simulated)/u)+filt
      p_names <- c(p_names, parties[i]) }
    
    df_cons <- data_simulated[as.logical(filt),]
    
    df_cons$pr_scores <- exp(as.matrix(df_cons[,5:11])%*%params[1:7])
    colnames(df_cons)[1] <- "id"
    aggr_sum <- aggregate(df_cons$pr_scores, by=list(df_cons[,1]), FUN = sum)
    colnames(aggr_sum)[1] <- "id"
    
    data_plot <- merge(df_cons, aggr_sum, by = "id")
    colnames(data_plot)[2] <- "party_cons_set"
    data_plot <- data_plot[data_plot$party_cons_set == party_code,]
    c_n <- paste0(parties[party_code]," (",paste(p_names,collapse=","),")")
    data_plot$party_cons_set <- rep(c_n,nrow(data_plot))
    colnames(data_plot)[4] <- "ideology"
    data_plot$Pr <- data_plot$pr_scores/data_plot$V1
    
    data_plot_all <- rbind(data_plot_all,data_plot)
  }

    data_plot_all$party_cons_set <- as.factor(data_plot_all$party_cons_set)
    
      plot <- ggplot(data=data_plot_all, aes(x=ideology, y=Pr,group=party_cons_set))+
      geom_line(aes(linetype=party_cons_set))+
      geom_point(aes(shape=party_cons_set))+
      theme(legend.position="right")+
      xlab(ideology_name)+
      scale_color_discrete(name = " ")+
      theme_bw()
  return(plot)
}



plot_cons_set_by_party_color <- function(df_ideologies,ideology_num,ideology_name,party_code,cons_sets,params){
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
  parties <- as.vector(df_ideologies$party)[1:6]
  
  data_plot_all <- data.frame()
  for (cons_set in cons_sets){
    filt <- rep(FALSE,nrow(data_simulated)/u)
    p_names <- c()
    for (i in unlist(cons_set)){
      filt <- rep(c(1:u)==i, nrow(data_simulated)/u)+filt
      p_names <- c(p_names, parties[i]) }
    
    df_cons <- data_simulated[as.logical(filt),]
    
    df_cons$pr_scores <- exp(as.matrix(df_cons[,5:11])%*%params[1:7])
    colnames(df_cons)[1] <- "id"
    aggr_sum <- aggregate(df_cons$pr_scores, by=list(df_cons[,1]), FUN = sum)
    colnames(aggr_sum)[1] <- "id"
    
    data_plot <- merge(df_cons, aggr_sum, by = "id")
    colnames(data_plot)[2] <- "party_cons_set"
    data_plot <- data_plot[data_plot$party_cons_set == party_code,]
    c_n <- paste0(parties[party_code]," (",paste(p_names,collapse=","),")")
    data_plot$party_cons_set <- rep(c_n,nrow(data_plot))
    colnames(data_plot)[4] <- "ideology"
    data_plot$Pr <- data_plot$pr_scores/data_plot$V1
    
    data_plot_all <- rbind(data_plot_all,data_plot)
   
  }
  
  data_plot_all$party_cons_set <- as.factor(data_plot_all$party_cons_set)
 
  plot <- ggplot(data=data_plot_all, aes_string(x="ideology", y="Pr",color="party_cons_set",group="party_cons_set"))+
    geom_line()+
    geom_point()+
    xlab(ideology_name)+
    ylab("Вероятность выбрать партию")+
    labs(color='Consideration Sets')+
    theme_bw()
  return(plot)
}


plot_boxplots <- function(df_probs,party){
  filt <- df_probs$choice==1
  filt <- filt+(df_probs$party==party)
  filt <- filt==2
  id <- df_probs[filt, ]$id
  b <- df_probs[df_probs$id %in% id, ]
  plot <-  ggplot(b, aes(x = party, y = probs_euc2))+
    geom_boxplot()+
    # ggtitle('Boxplots of distributions of probabilities if voter choose "United Russia"') +
    ylab("Pr")+
    xlab("Political Party")+
    theme_bw()
  return(plot)
}