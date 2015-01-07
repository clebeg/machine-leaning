##### 为黑客写的机器学习 R语言基础#####
#File: ufo.R
#Author: clebeg
#Date: 2014/9/18
#Purpose: 主要是为了研究ufo文本信息，得出美国ufo目击次数与时间的关系

##### maincode ####
#首先读取数据到内存
#文件使用制表符分割，没有标题行，并且不需要将字符串转换成因子
#需要将空字符作为缺失字符
ufo <- read.delim("basic2ml/data/ufo/ufo_awesome.tsv", sep="\t",
                  stringsAsFactors=FALSE, header=FALSE, na.strings="");
#没有属性名可读性会比较差，将ufo这个数据框每一列取个合适的名字
names(ufo) <- c("DateOccurred","DateReported","Location",
                "ShortDescription","Duration","LongDescription")
#从文档中知道第一列和第二列实际上是日期格式，那么R语言需要将字符串转换成日期
#和其他语言一样，要告诉R语言日期的格式，R语言才知道怎么样转换日期

ufo$DateOccurred <- as.Date(ufo$DateOccurred, format="%Y%m%d")

#查看时间格式不正确的日期
head(ufo[which(nchar(ufo$DateOccurred) != 8), 1])

#下面把错误的时间列找出来,which函数返回的是符合要求的数据的下标
rows.flag <- ifelse(nchar(ufo$DateOccurred)!=8|nchar(ufo$DateReported)!=8,
                  FALSE,TRUE)

#rows.flag <- which(nchar(ufo$DateOccurred)!=8|nchar(ufo$DateReported)!=8)
#选择所有好的列，向量长度的计算用length
ufo <- ufo[rows.flag, ]
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format="%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported, format="%Y%m%d")

#分解地点 按照城市,州 分解成城市和州
get.location <- function(cloc) {
  split.location <- tryCatch(strsplit(cloc, ",")[[1]], error=function(e) return(c(NA,NA)))
  clean.location <- gsub("^ ", "", split.location)
  if (length(clean.location) > 2) {
    return(c(NA,NA))
  } else {
    return(clean.location)
  }
}

city.state <- lapply(ufo$Location, get.location)

location.matrix <- do.call(rbind, city.state)
head(location.matrix)
ufo <- transform(ufo, USCity=location.matrix[,1], USState=location.matrix[,2],
                 stringsAsFactors=FALSE)
ufo$USState <- tolower(ufo$USState)

#美国各州的缩写
us.states<-c("ak","al","ar","az","ca","co","ct","de","fl","ga","hi","ia","id","il","in", 
             "ks","ky","la","ma","md","me","mi","mn","mo","ms","mt","nc","nd","ne","nh","nj","nm", 
             "nv","ny","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","va","vt","wa","wi","wv", 
             "wy")
ufo$USState <- us.states[match(ufo$USState, us.states)]
ufo$USCity[is.na(ufo$USState)] <- NA
ufo.us <- subset(ufo, !is.na(USState) & !is.na(DateOccurred))
head(ufo.us)
summary(ufo.us$DateOccurred)

if (require(ggplot2) == FALSE) {
  install.packages("ggplot2")
  require(ggplot2)
}

quick.hist <- ggplot(ufo.us, aes(x=DateOccurred))+geom_histogram()+scale_x_date(breaks='50 years')
ggsave(plot=quick.hist, filename="basic2ml/images/quick_hist2.png", height=6, width=8)
ufo.us <- subset(ufo.us, DateOccurred >= as.Date('1990-01-01'))
nrow(ufo.us)#nchar nuber of rows

#strftime 将时间按照一定的格式转换字符串
ufo.us$YearMonth <- strftime(ufo.us$DateOccurred, format="%Y-%m")
library(plyr)
sightings.counts <- ddply(ufo.us, .(USState, YearMonth), nrow)

#用sqldf性能比plyr性能更好，而且学习成本更加低，所以以后操作数据尽量用低成本的sqldf
library(sqldf)
sightings.counts <- sqldf("select USState, YearMonth, count(*) Numbers from \'ufo.us\' group by USState, YearMonth")

date.range <- seq.Date(from = as.Date(min(ufo.us$DateOccurred)),
                       to = as.Date(max(ufo.us$DateOccurred)),
                       by="1 month")
date.str <- strftime(date.range, format = "%Y-%m")

states.dates<-lapply(us.states,function(s) cbind(s,date.str))

states.dates<-data.frame(do.call(rbind,states.dates), stringsAsFactors=FALSE)
head(states.dates)

all.sightings<-merge(states.dates,
                     sightings.counts,
                     by.x=c("s","date.str"),
                     by.y=c("USState","YearMonth"),all=TRUE)
head(all.sightings)
names(all.sightings)<-c("State","YearMonth","Sightings")
all.sightings$Sightings[is.na(all.sightings$Sightings)]<-0
all.sightings$YearMonth<-as.Date(rep(date.range,length(us.states)))
all.sightings$State<-as.factor(toupper(all.sightings$State))


state.plot <- ggplot(all.sightings, aes(x = YearMonth,y = Sightings)) +
  geom_line(aes(color = "darkblue")) +
  facet_wrap(~State, nrow = 10, ncol = 5) +
  theme_bw() +
  scale_x_date(breaks = "10 years") +
  xlab("Time") +
  ylab("Number of Sightings")
ggsave(plot = state.plot, filename = file.path("basic2ml/images", "ufo_sightings.pdf"),
       width = 14, height = 8.5)