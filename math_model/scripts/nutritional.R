rm(list=ls())
#广东省总人口
gdtotalpeple <- 1059400000

library(lpSolve)
library(lpSolveAPI)
#求解线性方程组得到2020年的年产量吨
data <- read.csv("./data/dodata3.csv", header = FALSE)
data <- as.matrix(data)
#处理系数
xishu <- data[1, 1:(length(data[1,]) - 1)]
xishu <- xishu/sum(xishu)
#处理约束矩阵
yueshus <- data[3:length(data[,1]), 1:(length(data[1,]) - 1)]



#数理约束条件
h <- data[3:length(data[,1]),length(data[1,])]
h <- h*gdtotalpeple

yushu <- data[2, 1:(length(data[1,]) - 1)]

lprec <- make.lp(0, length(xishu))
set.objfn(lprec, xishu)
for (i in 1:length(yueshus[,1])) {
  add.constraint(lprec, yueshus[i,], ">=", h[i])
}
set.bounds(lprec, lower=yushu, columns=1:length(xishu))
set.bounds(lprec, upper=yushu[length(xishu)-1]*20, columns=length(xishu)-1)
set.bounds(lprec, upper=yushu[length(xishu)-2]*20, columns=length(xishu)-2)
solve(lprec)
result <- get.variables(lprec)

rm(lprec)
write.csv(result, "nutritionalResult.csv")