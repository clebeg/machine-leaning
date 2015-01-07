
rm(list=ls())
library(lpSolve)
library(lpSolveAPI)
#求解线性方程组得到2020年的年产量吨
data <- read.csv("./data/factor.csv", header = FALSE)
data <- as.matrix(data)
#处理系数
xishu <- data[1, 1:(length(data[1,]) - 1)]

#处理约束矩阵
yueshus <- data[4:length(data[,1]), 1:(length(data[1,]) - 1)]

shucha <- data[2, 1:(length(data[1,]) - 1)]

n2012 <- data[3, 1:(length(data[1,]) - 1)]
for (i in 1:length(yueshus[,1])) {
  for (j in 1:length(yueshus[1,])) {
    yueshus[i,j] <- yueshus[i,j] - shucha[j]
  }
}


#数理约束条件
h <- data[4:length(data[,1]),length(data[1,])]
h[1:(length(h)-6)] <- h[1:(length(h)-6)]/10000

alpha <- c(1,3,5,7,8,9)
confirm <- c(994060,31352.6,104686200,27400,404420,21800)

lprec <- make.lp(0, length(xishu))
set.objfn(lprec, -xishu)
for (i in 1:length(yueshus[,1])) {
  add.constraint(lprec, yueshus[i,], ">=", h[i])
}
set.bounds(lprec, lower=confirm, columns=alpha)
set.bounds(lprec, lower=n2012, columns=1:length(xishu))
set.bounds(lprec, upper=rep(10000000000,length(xishu)), 1:length(xishu))
solve(lprec)
result <- get.variables(lprec)

rm(lprec)
write.csv(result, "linearResult.csv")
