#File-Name:       probability-distribution.R
#Date:            2015-01-10 10:41:00
#Author:          Clebeg
#Email:           clebeg@163.com
#Purpose:         利用R语言学习概率与分布的基本知识（掌握基本知识才能走得远）
#Data Used:       
#Packages Used:   ggplot2, lubridate, reshape(注：可以使用install.packages('you-package-name')安装)
#Machine:         Clebeg'Lenovo Y485

# All source code is copyright (c) 2011, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/
# All rights reserved.

#下面的都是等可能的抽取
#在52张扑克牌中不防回的随机抽取四张扑克
sample(1:52, 4)

#掷一枚硬币10次
sample(c('H', 'T'), 10, replace=TRUE)

#投掷一枚骰子10次
sample(1:6, 10, replace=TRUE)

#下面的都是不等可能的抽取  sample(x, n, replace=TRUE, prob=y)
sample(c("成功", "失败"), 10, replace=T, prob=c(0.9,0.1))


#例 3.2.1 从一副完全打乱的52张扑克中取4张, 求以下事件的概率:
#1) 抽取的4张依次为红心A， 方块A， 黑桃A和梅花A的概率;P(n,k)
#2) 抽取的4张为红心A， 方块A， 黑桃A和梅花A的概率.c(n,k)

#R语言利用 prod() 函数表示连乘
1/prod(49:52)

#组合数在R中用choose()函数来求解
1/choose(52, 4)


#概率分布
#1) 贝努里分布: binom(1, p)  一次随机试验，只有成功和失败两个事件
#成功记为1，失败记为0，成功的概率为p 均值为p，方差为p(1-p)
#2) 二项分布: binom(n, p)  n次随机试验，每次试验只有成功和失败两个事件
#成功记为1，失败记为0，成功的概率为p 均值为np，方差为np(1-p)
#3) 多项分布: multinom(1, p)  n次随机试验，有k中可能结果，分别记为1到k
#概率分别是p1到pk，均值为np，方差为np(1-p)
#4) 负二项分布: nbinom(1, p)  一次随机试验，只有成功和失败两个事件
#成功记为1，失败记为0，成功的概率为p 一直试验到有k次成为止
#geom 几何分布  hyper超几何分布



#数据描述性统计
#mean(x, trim = 0, na.rm = FALSE) trim 表示去除离均值太远的值得比例 na.rm是否自动去除na
#如果你需要对矩阵的各行各列求均值需要使用apply()
#如果对象是数据框mean返回的就是按照各列求平均
#计算数据的加权平均值使用 weighted.mean(x, w, na.rm=FALSE)

#数据排序
x <- c(75.0, 66.9, 64.0, 63.5, 62.2, 62.2, 58.7, 47.4, NA, NA)
sort(x)
sort(x, na.last = TRUE)
sort(x, decreasing = TRUE)
sort(x, index.return = TRUE)
rank(x) 

#中位数median(x, na.rm = FALSE)

#百分位数
#百分位数是中位数的推广。数据从小到大排序后，对于 0 <= p < 1 分位数点
#当n是奇数时，就是np的整数部分加一的那个顺序统计量， 否则就是[np]与[np] + 1的平均值
#quantile(x, probs = seq(0, 1, 0.25), na.rm = FALSE,names = TRUE, type = 7, ...)
sort(x)
length(x)
quantile(x, 0.4, na.rm = TRUE)

#样本方差和标准差 var sd
var(x)
sd(x)
sd(x)^2

#样本变异系数 为cv <- 100*sd(w)/mean(w)
#样本校正平方和 css <- sum((w - mean(2))^2)
#样本未校正平方和 uss <- sum(w^2)

#样本极差： R <- max(w) - min(x)
#四分位差：样本上下四分位数之差就是四分位极差 R1 <- quantile(w, 0.75) - quantile(w, 0.25)
#样本标准误差: sm <- sd(w)/sqrt(n)

#偏度系数：总体偏度系数 = 3阶中心距 / 二阶中心距 的 3/2 次方
#样本偏度系数：n^2 * s3/(n-1)(n-2)s^3
#样本峰度系数：n^2(n+1)U4/(n-1)(n-2)(n-3)s^4 - 3((n-1)^2)/(n-2)(n-3WWW))


##数据的分布
#数据的数字特征刻画了数据的主要特征，而要对数据的总体情况作全面的描述，就要研究数据的分布。
#对数据分布的主要描述方法有：直方图、茎叶图、数据的理论分布即总体分布、正态性检验。

##数据统计图形展示
#对于数据分布，常用直方图histogram进行描述。将数据取值的范围分成若干区间（一般是等间距的）。
#在等间距的情况下，每个区间长度称为组距。考察数据落入每一区间的频数或频率，在每个区间上画一个矩形。
#它的高度可以是频数、频率、频率/组距，最后一种情况可以估计总体的概率密度。
w <- c(75.0, 64.0, 47.4, 66.9, 62.2, 62.2, 58.7, 63.5,
       66.6, 64.0, 57.0, 69.0, 56.9, 50.0, 72.0)
hist(w, freq = FALSE)
lines(density(w), color = "blue")
x <- 44:76
lines(x, dnorm(x, mean(x), sd(w)))

library(ggplot2)
ggplot(data.frame(x = w), aes(x = x)) + 
  geom_bar() +
  geom_density()

#上述直方图的概率密度曲线可以很方便的估计连续函数的分布，但是对于一般的样本，如何估计总体分布
#我们可以用经验分布函数：ecbf empirical distribution function
#经验分布函数的定义非常简单：对于x处的的值，经验分布函数取值为小于这个数的样本点个数/总样本个数
y <- ecdf(w)
plot(y, verticals = TRUE)
lines(x, pnorm(x, mean(w), sd(w)))
qqnorm(w)
qqline(w)

##正态性检验
#利用Shapiro-Wilk W 统计量作正态性检验，会得到p值，如果p值小于某个显著性水平，则任务样本不是来自正态总体。
shapiro.test(w)

#正态性ks检验