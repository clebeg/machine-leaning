#File-Name:       cluster_analysis.R
#Date:            2015-01-09 11:00:00
#Author:          Clebeg
#Email:           clebeg@163.com
#Purpose:         可视化地研究参议员的相似程度
#Data Used:       
#Packages Used:   ggplot2(注：可以使用install.packages('you-package-name')安装)
#Machine:         Clebeg'Lenovo Y485

#All source code is copyright (c) 2011, under the Simplified BSD License.
#For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php
#All images and materials produced by this code are licensed under the Creative Commons
#Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/
#All rights reserved.

#聚类分析是非监督学习的一种重要的分析方法。它可以解决如下类似问题：
#例如：通过调查问卷，你想知道那类人喜欢这个品牌。（为什么这群人喜欢这个品牌！）