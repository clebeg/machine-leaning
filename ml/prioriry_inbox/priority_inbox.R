rm(list=ls())
#设置当地为美国
Sys.setlocale('LC_TIME', "C")
#text mining 文本挖掘包
library(tm)
#将结果作为图形展示的包
library(ggplot2)

data.path <- "./email_classification/data/"
easyham.path <- paste(data.path, "easy_ham/", sep="")

#接下来，我们需要从这些邮件中抽取出
#收到信的时间
#发信人的IP地址、接受到的数据、
#主题、以及消息主体
#下面学习模块化开发的思路，先大后小
#我们需要将电子邮件数据变化成一个可用的数据集
#我们需要的特性，即我们矩阵的属性
#每一行就是从各个样本中抽取的属性值
#其实任何分析问题，都是一系列指标（属性），得到样本值分析
parse.email <- function(path) {
  #模块化开发，不管这事什么，
  #我们需要的是什么，先写上函数
  #然后我们再一个一个去实现
  #从路径中得到所有需要训练的文本，msg.full(path)
  full.msg <- msg.full(path)
  #从文本中的到收信的时间
  date <- get.date(full.msg)
  #从文本中得到发信人的IP地址
  from <- get.from(full.msg)
  #从邮件中得到主题
  subj <- get.subject(full.msg)
  #从邮件中得到消息主题内容
  msg <- get.massage(full.msg)
  return (c(date, from, subj, msg, path))
}

#将文本读入内存而已
#这里我们返回文本所有行并没有做任何处理
#不像之前的垃圾邮件分类器
#因为我们需要提取很多东西，从原始文本中
#接下来我们会写很多个函数去提取我们想要的内容
msg.full <- function(path) {
  file = file(path, open = "rt", encoding = "latin1")
  #读入文本到一个向量里面，其中向量的第i个位置对应第i行
  msg <- readLines(file)
  close(file)
  return(msg)
}

#msg.path <- paste(easyham.path, dir(easyham.path), sep = "")

#msg.vec <- msg.full(msg.path[1])


#grepl函数就是grep函数的推广，它会对向量中的每一个元素作用，然后返回
#logical 即true or false 不同于which返回数组下标，
#它将返回和作用向量一样长度的逻辑值
get.from <- function(msg.vec) {
  from <- msg.vec[grepl("From:", msg.vec)]
  from <- strsplit(from, '[":<>]')[[1]]
  from <- from[which(from != "" & from != " ")]
  return(from[grepl("@", from)][[1]])
}

get.massage <- function(msg.vec) {
  msg <- msg.vec[seq(which(msg.vec == "")[1] + 1, length(msg.vec), 1)]
  return (paste(msg, collapse = "\n"))
}

get.subject <- function(msg.vec) {
  subj <- msg.vec[grepl("Subject: ", msg.vec)]
  if (length(subj) > 0) {
    return(strsplit(subj, "Subject")[[1]][2])
  } else {
    return("")
  }
}

#在处理数据的过程中你不犯错就不会前进，错误总是会有的，
#真实的世界你不可能一下子就清楚，我们必须不停地去发现，然后改进
#现在我们已经获得了四分之三的属性，但是还有一个很重要，
#但是确也相当重要的属性：日期和时间
#它为什么难提取呢？原因有两个：
#一、处理时间通常是很痛苦的，不同的编程语言对时间有不同的理解
#二、邮件里面的时间多种多样，处理很烦
get.date <- function(msg.vec) {
  date.grep <- grepl("^Date: ", msg.vec)
  date.grepl <- which(date.grep == TRUE)
  date <- msg.vec[date.grepl[1]]
  date <- strsplit(date, "\\+|\\-|: ")[[1]][2]
  #gsub 将符合前面模式的事务，替换成第二个字符
  date <- gsub("^\\s+|\\s+$", "", date)
  return(strtrim(date, 25))
}

easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != 'cmds')]
easyham.parse <- lapply(easyham.docs, 
                        function(p) 
                          parse.email(paste(easyham.path, p, sep="")))

ehparse.matrix <- do.call(rbind, easyham.parse)
allparse.df <- data.frame(ehparse.matrix, stringsAsFactors = FALSE)
names(allparse.df) <- c("Date", "From.EMail", "Subject", "Message", "Path")

#下面将时间字符串转换为真正的时间格式
#因为邮件中包含两种时间格式，让我们下面这样处理
date.converter <- function(dates, pattern1, pattern2) {
  pattern1.convert <- strptime(allparse.df$Date, pattern1)
  pattern2.convert <- strptime(allparse.df$Date, pattern2)
  pattern1.convert[is.na(pattern1.convert)] <- pattern2.convert[is.na(pattern1.convert)]
  return(pattern1.convert)
}

pattern1 <- "%a, %d %b %Y %H:%M:%S"
pattern2 <- "%d %b %Y %H:%M:%S"

allparse.df$Date <- date.converter(allparse.df$Date, pattern1, pattern2)

allparse.df$Subject <- tolower(allparse.df$Subject)
allparse.df$From.EMail <- tolower(allparse.df$From.EMail)
priority.df <- allparse.df[with(allparse.df, order(Date)), ]
priority.train <- priority.df[1:(round(nrow(priority.df) / 2)),]

library(plyr)
from.weight <- ddply(priority.train, .(From.EMail), summarise, Freq = length(Subject))