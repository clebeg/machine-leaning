#File-Name:       var-analysis.R
#Date:            2015-01-13 16:47:00
#Author:          Clebeg
#Email:           clebeg@163.com
#Purpose:         利用R语言学习方差分析
#Data Used:       
#Packages Used:   (注：可以使用install.packages('you-package-name')安装)
#Machine:         Clebeg'Lenovo Y485

# All source code is copyright (c) 2011, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/
# All rights reserved.

#title:方差分析
#1.1 单因素方差分析
# q1.1.1 利用四种不同配方的材料A1, A2, A3, A4生成处理的元件，测得使用寿命如下
A1 <- c(1600, 1610, 1650, 1680, 1700, 1700, 1780)
A2 <- c(1500, 1640, 1400, 1700, 1750)
A3 <- c(1640, 1550, 1600, 1620, 1640, 1600, 1740, 1800)
A4 <- c(1510, 1520, 1530, 1570, 1640, 1600)

lamp <- data.frame(
  x = c(A1, A2, A3, A4), 
  a = factor(c(rep('A1', length(A1)), rep('A2', length(A2)), rep('A3', length(A3)),rep('A4', length(A4))))
)
#请问这四种配方有无显著差异？

aov <- aov(x ~ a, lamp)
summary(aov)
