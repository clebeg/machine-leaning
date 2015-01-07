rm(list = ls())
library(ggplot2)
win.graph(width = 4.875, height = 2.5, pointsize = 8)
rwalk <- read.table("E:/r_project/learning/time_sequence/Datasets/rwalk.dat", header=TRUE, quote="\"")

ggplot(rwalk, aes(x = row.names(rwalk), y = rwalk )) + geom_line()
plot(rwalk, type = 'o')
