library(e1071)

install.packages("reshape")
#获得3000个训练样本
simData=function(radius,width,distance,sample_size)
{
  # runif 是 random uniform的缩写，默认是0，1上的均匀分布  
  # 比如说 U3 <- runif (1000，1，2)  那就是1，2上均匀分布
  aa1=runif(sample_size/2) #生成sample_size/2个服从0到1上的均匀分布
  aa2=runif(sample_size/2)
  
  rad=(radius-width/2)+width*aa1
  theta=pi*aa2
  x=rad*cos(theta)
  y=rad*sin(theta)
  label=1*rep(1,length(x))
  
  x1=rad*cos(-theta)+rad
  y1=rad*sin(-theta)-distance
  label1=-1*rep(1,length(x1))
  
  n_row=length(x)+length(x1)
  
  data=matrix(rep(0,3*n_row),nrow=n_row,ncol=3)
  data[,1]=c(x,x1)
  data[,2]=c(y,y1)
  data[,3]=c(label,label1)
  
  return(data)
  
}
dataSim <- simData(radius=10,width=6,distance=-6,sample_size=3000)

colnames(dataSim) <- c("x","y","label")
dataSim <- as.data.frame(dataSim)

m1 <- svm(label ~ x+y, data = dataSim, cross = 10,type = "C-classification", kernel = "sigmoid")
m1
summary(m1)
pred1<-fitted(m1)

#table函数可以将两个向量对比，最后把汇总结果给出来，即以位置为基准，
#以第一个向量的不同情况作为行，第二个向量不同类型作为列，将每一种结果出现的次数给出
table(pred1,dataSim[,3])


# 结果可视化
linear.svm.fit <- svm(label ~ x + y, data = dataSim, kernel ='linear')
with(dataSim, mean(label == ifelse(predict(linear.svm.fit) > 0,1, -1)))

polynomial.svm.fit <- svm(label ~ x + y, data = dataSim, kernel ='polynomial')
with(dataSim, mean(label == ifelse(predict(polynomial.svm.fit) >0, 1, -1)))

radial.svm.fit <- svm(label ~ x + y, data = dataSim, kernel ='radial')
with(dataSim, mean(label == ifelse(predict(radial.svm.fit) > 0,1, -1)))

sigmoid.svm.fit <- svm(label ~ x + y, data = dataSim, kernel ='sigmoid')
with(dataSim, mean(label == ifelse(predict(sigmoid.svm.fit) > 0,1, -1)))

df <- cbind(dataSim,
            data.frame(LinearSVM = ifelse(predict(linear.svm.fit) > 0, 1, -1),
                       PolynomialSVM = ifelse(predict(polynomial.svm.fit) > 0, 1, -1),
                       RadialSVM = ifelse(predict(radial.svm.fit) > 0, 1, -1),
                       SigmoidSVM = ifelse(predict(sigmoid.svm.fit) > 0, 1, -1)))
library("reshape")
predictions <- melt(df, id.vars = c('x', 'y'))
library('ggplot2')
ggplot(predictions, aes(x = x, y = y, color = factor(value))) +
  geom_point() +
  facet_grid(variable ~ .)



