#File-Name:       simple_decryption.R
#Date:            2015-01-08 15:50:00
#Author:          Clebeg
#Email:           clebeg@163.com
#Purpose:         ���ù�Ʊ������ģ�����ɷַ����㷨
#Data Used:       pca/stock_prices.csv, pca/DJI.csv
#Packages Used:   ggplot2, lubridate, reshape(ע������ʹ��install.packages('you-package-name')��װ)
#Machine:         Clebeg'Lenovo Y485

# All source code is copyright (c) 2011, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/
# All rights reserved.

#PCA ��ʾ�������ɷַ�����Pricipal Components Analysis����
#PCA ����Ҫ˼·�ǣ�����һ��ԭʼ����ͬ��ά�ȵ������ݼ�������ÿһ�а���ԭʼ������Ϣ�Ķ��٣���������
#�Ӵ�С��������ڵ�һ�ľ��ǵ�һ���ɷ֣����Ϊ���ɷ֣���
#���ɷ���Ȼ��ʧ�˺ܶ���Ϣ������ȴ������ԭʼ����ĸ��Ӷȣ����һ������ܵİ���ԭʼ���ݺܶ���Ϣ
#�����ǵ���������ÿһ�ж����к�ǿ��������س̶ȵ�ʱ�����ɷַ����ǳ���Ч��

#��һ�������Ʊ����
rm(list = ls())
prices <- read.csv('pca/stock_prices.csv')

#ԭʼ�����е�ʱ���ʽ�����ǲ�ϲ����ͨ��ʹ��һ���°��������ɽ����һ�㡣
#����ѧϰ����°����÷���lubridate
#�����������һ������ ymd��Ĭ�Ͻ�"yyyy-mm-dd"���ָ�ʽ���ַ���ת�������ڶ���
#install.packages('lubridate')
library('lubridate')
prices <- transform(prices, Date = ymd(Date))

#install('reshape')
library('reshape')
date.stock.matrx <- cast(prices, Date ~ Stock, value = 'Close')
prices <- subset(prices, Date != ymd('2012-02-01'))
prices <- subset(prices, Stock != 'DDR')
date.stock.matrix <- cast(prices, Date ~ Stock, value = 'Close')

cor.matrix <- cor(date.stock.matrix[, 2:ncol(date.stock.matrix)])
correlations <- as.numeric(cor.matrix)

ggplot(data.frame(Correlaton = correlations),
				aes(x = Correlation, fill = 1)) +
	geom_density() +
	opts(legend.position = 'none')
	
#��R��������ʹ�����ɷַ����㷨��ֻҪʹ��������� princomp ���ɷ�ÿ�����ʵ�ǰ�ĸ���ĸ
pca <- princomp(date.stock.matrix[, 2:ncol(date.stock.matrix)])

principal.component <- pca$loadings[, 1]

loadings <- as.numeric(principal.component)

ggplot(data.frame(Loading = loadings),
			aes(x = Loading, fill = 1)) +
	geom_density() +
	opts(legend.position = 'none')

#����predict�������Ի�õ�һ���ɷ�
market.index <- predict(pca)[, 1]

#����������Dow Jones Index �г�ָ���������ǵ�ָ����֮�Աȣ�����Ч�����
dji.prices <- read.csv('pca/DJI.csv')
dji.prices <- transform(dji.prices, Date = ymd(Date))

#��ȡ���Ǹ���Ȥ�����ڶ�
dji.prices <- subset(dji.prices, Date > ymd('2001-12-31'))
dji.prices <- subset(dji.prices, Date != ymd('2002-02-01'))

dji <- with(dji.prices, rev(Close))
dates <- with(dji.prices, rev(Date))

comparison <- data.frame(Date = dates, MarketIndex = market.index, DJI = dji)
ggplot(comparison, aes(x = MarketIndex, y = DJI)) +
 geom_point() +
 geom_smooth(method = 'lm', se = FALSE)
 
comparison <- transform(comparison, MarketIndex = -1 * MarketIndex)

ggplot(comparison, aes(x = MarketIndex, y = DJI)) +
 geom_point() +
 geom_smooth(method = 'lm', se = FALSE)
 
alt.comparison <- melt(comparison, id.vars = 'Date')
names(alt.comparison) <- c('Date', 'Index', 'Price')

ggplot(alt.comparison, aes(x = Date, y = scale(Price), group = scale(Index), color = Index)) +
	geom_point() +
	geom_line()