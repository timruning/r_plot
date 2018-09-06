setwd("E:\\workspace\\news\\cheatflow\\plot")
library(dplyr)
library(ggplot2)

data=read.table("E:\\workspace\\news\\cheatflow\\dataConsole\\result\\event5_channel_uid_pageSourc_statistic_20171121",col.names = c("channel","pageSource","num"))
tmp=data%>%
  group_by(channel)%>%
  summarise(sum=sum(num))

data1=left_join(data,tmp)
data1$per=data1$num/data1$sum
data1$pageSource=as.factor(data1$pageSource)
data1$channel=as.factor(data1$channel)
data1%>%
  ggplot(aes(x=channel,y=per,fill=pageSource))+
  geom_bar(stat = "identity")
