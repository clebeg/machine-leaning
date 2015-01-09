#File-Name:       pca.R
#Date:            2015-01-08 15:50:00
#Author:          Clebeg
#Email:           clebeg@163.com
#Purpose:         利用股票数据来模拟主成分分析算法
#Data Used:       pca/stock_prices.csv, pca/DJI.csv
#Packages Used:   ggplot2, lubridate, reshape(注：可以使用install.packages('you-package-name')安装)
#Machine:         Clebeg'Lenovo Y485

# All source code is copyright (c) 2011, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/
# All rights reserved.

#PCA 表示的是主成分分析（Pricipal Components Analysis）。
#PCA 的主要思路是：创建一个原始数据同等维度的新数据集，根据每一列包含原始数据信息的多少，进行排序
#从大到小排序后。排在第一的就是第一主成分（简称为主成分）。
#主成分虽然损失了很多信息，但是却降低了原始问题的复杂度，而且还尽可能的包含原始数据很多信息
#尤其是当现有数据每一列都是有很强的线性相关程度的时候，主成分方法非常有效。

#第一步导入股票数据
rm(list = ls())
prices <- read.csv('pca/stock_prices.csv')

#原始数据中的时间格式，我们不喜欢，通过使用一个新包可以轻松解决这一点。
#下面学习这个新包的用法：lubridate
#这个包里面有一个函数 ymd：默认将"yyyy-mm-dd"这种格式的字符串转换成日期对象。
#install.packages('lubridate')
library('lubridate')
prices <- transform(prices, Date = ymd(Date))

#install.packages('reshape')
library('reshape')

#cast 函数用户 cat函数的第二个参数波浪号之前指定行，波浪号之后指定列，最后value指定行列的值
#波浪号之后的每一种情况作为列名
date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

date.stock.matrix <- na.omit(date.stock.matrix)

cor.matrix <- cor(date.stock.matrix[, 2:ncol(date.stock.matrix)])
correlations <- as.numeric(cor.matrix)

library(ggplot2)
ggplot(data.frame(Correlation = correlations),
				aes(x = Correlation, fill = 1)) +
	geom_density() +
	theme(legend.position = 'none')
	
#在R语言里面使用主成分分析算法，只要使用这个函数 princomp 主成分每个单词的前四个字母
pca <- princomp(date.stock.matrix[, 2:ncol(date.stock.matrix)])

principal.component <- pca$loadings[, 1]

loadings <- as.numeric(principal.component)

ggplot(data.frame(Loading = loadings),
			aes(x = Loading, fill = 1)) +
	geom_density() +
	theme(legend.position = 'none')

#利用predict函数可以获得第一主成分
market.index <- predict(pca)[, 1]

our.index <- data.frame(Date = date.stock.matrix$Date,
                        MarketIndex = market.index)

#利用有名的Dow Jones Index 市场指数来拿我们的指数与之对比，看看效果如何
dji.prices <- read.csv('pca/DJI.csv')

library(sqldf)

comparison <- sqldf("select a.Date, a.MarketIndex, b.close as DJI from 'our.index' a, 'dji.prices' b where a.Date = b.Date")
comparison <- transform(comparison, MarketIndex = scale(MarketIndex), DJI = scale(DJI))
ggplot(comparison, aes(x = MarketIndex, y = DJI)) +
 geom_point() +
 geom_smooth(method = 'lm', se = FALSE)
 
comparison <- transform(comparison, MarketIndex = -1 * MarketIndex)

ggplot(comparison, aes(x = MarketIndex, y = DJI)) +
 geom_point() +
 geom_smooth(method = 'lm', se = FALSE)
 
alt.comparison <- melt(comparison, id.vars = 'Date')
names(alt.comparison) <- c('Date', 'Index', 'Price')


ggplot(alt.comparison, aes(x = Date, y = Price, group = Index, color = Index)) +
	geom_point() +
	geom_line()