setwd("E:\\workspace\\news\\cheatflow\\masterTimeDis")
library(ggplot2)
library(dplyr)

data=read.table("E:\\workspace\\news\\cheatflow\\masterTimeDis\\plot_18310579728"
                ,col.names = c("label","master","uid","time","index","click"))

data$uid<-as.factor(data$uid)
tmp=data%>%
  group_by(uid)%>%
  summarise(sum=sum(click))

tmp1<- tmp%>%filter(sum>400)
uidset=tmp1$uid
data1<-data%>%filter(uid %in% uidset)

data1 %>% ggplot(aes(x=uid,y=index,color=click))+
  geom_point(shape=0)
