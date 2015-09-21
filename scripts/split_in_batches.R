setwd("/Users/ae/Documents/Duke_SS/pinterest/batches")
df_full <- read.csv("/Users/ae/Documents/Duke_SS/pinterest/batches/sample2000.csv",stringsAsFactors = F )
df_full <- df_full[order(df_full$board_id,df_full$pin_no),]
df_full$index <- 1:nrow(df_full)

for (i in 0:79) {
  df <- df_full[((i*25)+1):((i*25)+25), ]
  write.csv(df[c(1,6,11,16,21),], file = paste('batch_',i*5+1,'.csv', sep=''))
  write.csv(df[c(2,7,12,17,22),], file = paste('batch_',i*5+2,'.csv', sep=''))
  write.csv(df[c(3,8,13,18,23),], file = paste('batch_',i*5+3,'.csv', sep=''))
  write.csv(df[c(4,9,14,19,24),], file = paste('batch_',i*5+4,'.csv', sep=''))
  write.csv(df[c(5,10,15,20,25),], file = paste('batch_',i*5+5,'.csv', sep=''))
}

recreate <- read.csv("batch_1.csv")
for (i in 2:400) { 
  add <- read.csv(paste('batch_',i,'.csv', sep=""))
  recreate <- rbind(recreate,add)
}
recreate <- recreate[order(recreate$board_id,recreate$pin_no),]
table(table(recreate$index))

save(df_full, file="df.RData")
