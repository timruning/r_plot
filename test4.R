#羊头用户点击率分布

setwd("E:\\workspace\\news\\cheatflow\\dataConsole\\userdata")

data=read.table("E:\\workspace\\news\\cheatflow\\dataConsole\\userdata\\master_uid_ctr_dis",col.names = c("master","ctr","num"))
library(dplyr)
library(ggplot2)
tmp=data%>%
  group_by(master)%>%
  summarise(sum=sum(num))
data1<-left_join(data,tmp)
tmp1<-tmp%>%filter(sum>500)
data1<-data1%>%filter(master %in% tmp1$master)
data1$per=data1$num/data1$sum
data1$master<-as.factor(data1$master)
data1%>%ggplot(aes(x=ctr,y=per,color=master))+
  geom_point()+
  geom_line()
