library(dplyr)
library(ggplot2)
data=read.table("E:\\workspace\\news\\cheatflow\\dataConsole\\result\\lingerTimeStatistic3",col.names = c("channel",'lingerTime','clickNum'))
tail(data)

tmp=data%>%
  group_by(channel)%>%
  summarise(sum=sum(clickNum))

data=left_join(data,tmp)
data$pre=data$clickNum/data$sum

data_106<-data%>%filter(channel %in% c(106))
data_106<-subset(data_106,lingerTime>0)
#rm(data)
tail(data_106)

data_106 %>% ggplot(aes(x=lingerTime,y=pre))+
  geom_point()+
  geom_line()+
  geom_smooth()

data_18<-data%>%filter(channel %in% c(18,9,999,32))

data_18 %>% ggplot(aes(x=lingerTime,y=pre,color=data_18$channel))+
  geom_point()+
  geom_line()

data_999<-data%>%filter(channel %in% c(999))

data_999 %>% ggplot(aes(x=lingerTime,y=pre))+
  geom_point()+
  geom_line()+
  geom_smooth()

data_32<-data%>%filter(channel %in% c(32))

data_32 %>% ggplot(aes(x=lingerTime,y=pre))+
  geom_point()+
  geom_line()+
  geom_smooth()


data_89<-data%>%filter(channel %in% c(89))

data_89 %>% ggplot(aes(x=lingerTime,y=pre))+
  geom_point()+
  geom_line()+
  geom_smooth()

data_11<-data%>%filter(channel %in% c(11))

data_11 %>% ggplot(aes(x=lingerTime,y=pre))+
  geom_point()+
  geom_line()+
  geom_smooth()

data_83<-data%>%filter(channel %in% c(83))

data_83 %>% ggplot(aes(x=lingerTime,y=pre))+
  geom_point()+
  geom_line()+
  geom_smooth()

data_84<-data%>%filter(channel %in% c(84))

data_84 %>% ggplot(aes(x=lingerTime,y=pre))+
  geom_point()+
  geom_line()+
  geom_smooth()

data_103<-data%>%filter(channel %in% c(103))

data_103 %>% ggplot(aes(x=lingerTime,y=pre))+
  geom_point()+
  geom_line()+
  geom_smooth()

data_104<-data%>%filter(channel %in% c(104))

data_104 %>% ggplot(aes(x=lingerTime,y=pre))+
  geom_point()+
  geom_line()+
  geom_smooth()

data_68<-data%>%filter(channel %in% c(68))

data_68 %>% ggplot(aes(x=lingerTime,y=pre))+
  geom_point()+
  geom_line()+
  geom_smooth()

data_69<-data%>%filter(channel %in% c(69))

data_69 %>% ggplot(aes(x=lingerTime,y=pre))+
  geom_point()+
  geom_line()+
  geom_smooth()

data_67<-data%>%filter(channel %in% c(67))

data_67 %>% ggplot(aes(x=lingerTime,y=pre))+
  geom_point()+
  geom_line()+
  geom_smooth()

data_75<-data%>%filter(channel %in% c(75))
data_75 %>% ggplot(aes(x=lingerTime,y=pre))+
  geom_point()+
  geom_line()+
  geom_smooth()
data_2<-data%>%filter(channel %in% c(2))
data_2 %>% ggplot(aes(x=lingerTime,y=pre))+
  geom_point()+
  geom_line()+
  geom_smooth()
data_9<-data%>%filter(channel %in% c(9))
data_9 %>% ggplot(aes(x=lingerTime,y=pre))+
  geom_point()+
  geom_line()+
  geom_smooth()

