rm(list=ls())

all_vegetable <- read.csv("./data/USDA/vegetable/全国output.csv", fileEncoding="utf8", row.names = 1, header = TRUE)
all_fruit <- read.csv("./data/USDA/fruit/全国output.csv", fileEncoding="utf8", header = TRUE,row.names = 1)
all_vegetable['白菜','X2009'] <- NA
#计算产量贡献率
options(digits =2)
contribute_fruit <- all_fruit

for (i in 1:length(contribute_fruit[1,])) {
  contribute_fruit[,i] <- contribute_fruit[,i]/contribute_fruit['水果',i]
  
}
mc <- vector();
for (i in 1:length(contribute_fruit[,1])) {
  mc[i] <- mean(as.numeric(contribute_fruit[i,]), na.rm=TRUE) 
}

contribute_fruit <- cbind(contribute_fruit, avgc = mc)
complete_fruit <- all_fruit
#补全数据 contribute_fruit
for (i in 1:length(complete_fruit[,1])) {
  for (j in 1:length(complete_fruit[1,])) {
    if(is.na(complete_fruit[i,j]) == TRUE) {
      complete_fruit[i,j] = complete_fruit['水果',j] * contribute_fruit[i,'avgc']
    }
  }
}
write.csv(contribute_fruit, file="./data/result/contribute_fruit.csv")
write.csv(complete_fruit, file="./data/result/complete_fruit.csv")

contribute_vegetable <- all_vegetable;
for (i in 1:length(contribute_vegetable[1,])) {
  contribute_vegetable[,i] <- contribute_vegetable[,i]/contribute_vegetable['蔬菜',i]
}
mc <- vector();
for (i in 1:length(contribute_vegetable[,1])) {
  mc[i] <- mean(as.numeric(contribute_vegetable[i,]), na.rm=TRUE) 
}
contribute_vegetable <- cbind(contribute_vegetable, avgc = mc)
complete_vegetable <- all_vegetable
for (i in 1:length(complete_vegetable[,1])) {
  for (j in 1:length(complete_vegetable[1,])) {
    if(is.na(complete_vegetable[i,j]) == TRUE) {
      complete_vegetable[i,j] = complete_vegetable['蔬菜',j] * contribute_vegetable[i,'avgc']
    }
  }
}
write.csv(contribute_vegetable, file="./data/result/contribute_vegetable.csv")
write.csv(complete_vegetable, file="./data/result/complete_vegetable.csv")
sum(mc, na.rm=TRUE)
