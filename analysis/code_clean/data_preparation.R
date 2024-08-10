library(dummies)
library(reshape)
library(xtable)

setwd("/home/anastasia/Documents/Disser/data")

load("2012 data no labels.RData")
data <- x

data <- data[order(data$vid),]

#to get distribution of respondents' choice
table(data$v81)

#to filter data
data <- data[!is.na(data$v81),]
data <- data[data$v81<21, ]
data <- data[data$v81!=3, ] # drop Patriots of Russia
table(data$v81)

library(reshape2)
data_inf <- data[,c("vid","v18.1","v18.2","v18.3","v18.4")]
colnames(data_inf) <- c("vid","TV","Radio","Newspapers","Internet")
data_inf <- melt(data_inf, id=c("vid"))
data_inf <- data_inf[!is.na(data_inf$value),]
length(unique(data_inf$vid))

# Rotate v81
v81_dummy <- dummy(data$v81, sep="_")
v81_dummy <- melt(v81_dummy)
v81_dummy <- v81_dummy[order(v81_dummy$Var1, v81_dummy$Var2),]

df <- v81_dummy
colnames(df) <- c("vid","party_code","choice")

#1.JR, 2.LDPR, 3.CPRF, 4.Yabloko, 5.UR, 6.RightCause

# Rotate v40
v40_dummy <- melt(data[,c('vid','v40.4','v40.3','v40.2','v40.6','v40.1','v40.5')], id='vid')
v40_dummy <- v40_dummy[order(v40_dummy$vid),]
v40_dummy$value <- ifelse(v40_dummy$value>10,NA,v40_dummy$value)
v40_dummy$value <- ifelse(is.na(v40_dummy$value),mean(v40_dummy$value,na.rm=T),v40_dummy$value) # replace all the values 96,97,98 with mean

df <- cbind(df,left_right_party = v40_dummy$value)


lr <- data[,c("vid","v39")]
lr$v39 <-ifelse(lr$v39>10,NA,lr$v39) 
lr$v39 <-ifelse(is.na(lr$v39),mean(lr$v39,na.rm=T),lr$v39)
hist(lr$v39,breaks=10,main="Distribution of Left-Right Scale for Respondent")
lr <- do.call("rbind", replicate(6, lr, simplify = FALSE))
lr <- lr[order(lr$vid),]
df["left_right_resp"] <- lr$v39

# Rotate v43
# 1:UR, 2:CPRF, 4:JR, 5:LDPR, 6:Yabloko, 20:RightCause
table(data$v43)
v43_dummy <- dummy(data$v43, sep="_")
colnames(v43_dummy)
v43_dummy <- v43_dummy[,c("v43_4","v43_5","v43_2","v43_6","v43_1","v43_20")]
v43_dummy <- melt(v43_dummy)
v43_dummy <- v43_dummy[order(v43_dummy$X1),]

df <- cbind(df,strong_aff_1 = v43_dummy$value)

# Rotate v46
table(data$v46)
v46_dummy <- dummy(data$v46, sep="_")
colnames(v46_dummy)
v46_dummy <- v46_dummy[,c("v46_4","v46_5","v46_2","v46_6","v46_1","v46_20")]
v46_dummy <- melt(v46_dummy)
v46_dummy <- v46_dummy[order(v46_dummy$X1),]

df <- cbind(df,strong_aff_2 = v46_dummy$value)

df['elect_viability'] <- rep(c(0.1324*450,0.1167*450, 0.1919*450,0.0343*450, 0.4932*450, 0.006*450),nrow(df)/6)

df['media_repr_count'] <- rep(c(13878, 12214, 22138, 7332, 44857, 6908),nrow(df)/6)

df['media_repr_mean'] <- rep(c(447.67741, 394, 714.129, 236.516, 1447, 222.8387),nrow(df)/6)

data_inf <- do.call("rbind", replicate(6, data[,c("vid","v18.1","v18.2","v18.3","v18.4")], simplify = FALSE))
data_inf <- data_inf[order(data_inf$vid),]
data_inf$vid <- NULL
colnames(data_inf) <- c('main_inf_source_tv','main_inf_source_radio','main_inf_source_newspapers','main_inf_source_internet')
data_inf[!is.na(data_inf)] <-1 
data_inf[is.na(data_inf)] <-0
df <- cbind(df,data_inf)

# Rotate and recode other variables
      
vect <- data[,c('vid','v22','v24','v31.1', 'v31.2',
                'v94.1','v94.2','v95.1','v95.2','v95.3','v95.4','v95.5','v95.6','v95.7','v95.9','v87','v90','v116','v117','v132')]
vect <- do.call("rbind", replicate(6, vect, simplify = FALSE))
vect <- vect[order(vect$vid),]
df <- cbind(df,vect[,-c(1)])

write.csv(df,'prepared_data.csv', row.names = F)

