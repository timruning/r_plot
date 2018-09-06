library(ggplot2)
library(dplyr)

data=read.csv("E:\\workspace\\news\\cheatflow\\dataConsole\\data\\channel_user_click_dis.CSV"
              ,col.names = c("channel","clickNum","userNum"))

data=data%>%filter(channel%in%c(2,999,36,37,10,87,11,83,19,9,67,84,77,25,33,104,103,18,101,34,28,14,31,3,29,108,106))
data$channel=as.factor(data$channel)
data$clickNum=as.factor(data$clickNum)
tmp=data%>%
  group_by(channel)%>%
  summarise(sum=sum(userNum))

data<-left_join(data,tmp)
data$per=data$userNum/data$sum
head(data)

data %>% ggplot(aes(x=channel))+
  geom_bar(aes(y=per,fill=data$clickNum),stat = "identity")

