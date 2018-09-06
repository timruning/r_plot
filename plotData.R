setwd("E:\workspace\news\cheatflow\plot")
library(ggplot2)
library(data.table)
library(dplyr)
data<-read.csv("E:\\workspace\\news\\cheatflow\\dataConsole\\result\\userTimeDis20171018",sep='\t',col.names=c('channel','hour','num','sum'))
data$channel<-as.factor(data$channel)

data1<-data%>%
  filter(channel %in% c('18','33','999'))
data1$num<-as.numeric(data1$num)
data1$sum<-as.numeric(data1$sum)
data1$ratio<-data1$num/data1$sum
data1 %>% ggplot(aes(x=hour,y=ratio,colour=channel))+
  geom_point()+geom_line()+geom_smooth()
library(dplyr)
