##### Ϊ�ڿ�д�Ļ���ѧϰ R���Ի���#####
#File: ufo.R
#Author: clebeg
#Date: 2014/9/18
#Purpose: ��Ҫ��Ϊ���о�ufo�ı���Ϣ���ó�����ufoĿ��������ʱ��Ĺ�ϵ

##### maincode ####
#���ȶ�ȡ���ݵ��ڴ�
#�ļ�ʹ���Ʊ����ָû�б����У����Ҳ���Ҫ���ַ���ת��������
#��Ҫ�����ַ���Ϊȱʧ�ַ�
ufo <- read.delim("basic2ml/data/ufo/ufo_awesome.tsv", sep="\t",
                  stringsAsFactors=FALSE, header=FALSE, na.strings="");
#û���������ɶ��Ի�Ƚϲ��ufo������ݿ�ÿһ��ȡ�����ʵ�����
names(ufo) <- c("DateOccurred","DateReported","Location",
                "ShortDescription","Duration","LongDescription")
#���ĵ���֪����һ�к͵ڶ���ʵ���������ڸ�ʽ����ôR������Ҫ���ַ���ת��������
#����������һ����Ҫ����R�������ڵĸ�ʽ��R���Բ�֪����ô��ת������

ufo$DateOccurred <- as.Date(ufo$DateOccurred, format="%Y%m%d")

#�鿴ʱ���ʽ����ȷ������
head(ufo[which(nchar(ufo$DateOccurred) != 8), 1])

#����Ѵ����ʱ�����ҳ���,which�������ص��Ƿ���Ҫ������ݵ��±�
rows.flag <- ifelse(nchar(ufo$DateOccurred)!=8|nchar(ufo$DateReported)!=8,
                  FALSE,TRUE)

#rows.flag <- which(nchar(ufo$DateOccurred)!=8|nchar(ufo$DateReported)!=8)
#ѡ�����кõ��У��������ȵļ�����length
ufo <- ufo[rows.flag, ]
ufo$DateOccurred <- as.Date(ufo$DateOccurred, format="%Y%m%d")
ufo$DateReported <- as.Date(ufo$DateReported, format="%Y%m%d")

#�ֽ�ص� ���ճ���,�� �ֽ�ɳ��к���
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

#�������ݵ���д
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

#strftime ��ʱ�䰴��һ���ĸ�ʽת���ַ���
ufo.us$YearMonth <- strftime(ufo.us$DateOccurred, format="%Y-%m")
library(plyr)
sightings.counts <- ddply(ufo.us, .(USState, YearMonth), nrow)

#��sqldf���ܱ�plyr���ܸ��ã�����ѧϰ�ɱ����ӵͣ������Ժ�������ݾ����õͳɱ���sqldf
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